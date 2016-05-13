module Main exposing (..)

import Array exposing (Array)
import Html.App as Html
import Html exposing (Html, button, div, h1, h3, h6, li, text, ul)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=), decodeString)
import Maybe exposing (andThen)
import Process
import Task
import WebSocket


--
-- Wiring
--


main : Program Never
main =
  Html.program
    { init = init 5             -- nbSlots
                  2             -- scrollSpeed
                  darthSidious  -- first jedi to fetch
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
  WebSocket.listen "ws://localhost:4000" SetWorld


--
-- Models
--


type alias Model =
  { -- Obi-Wan's current location
    world:Maybe World
    -- The slots in view, which may contain dark jedis
  , jediSlots:Array (Maybe Jedi)
    -- Current scroll position. When we fire off a request to fetch a new jedi,
    -- we store the current index of the slot in which to inject the new jedi as
    -- well as the current scroll position. When the request completes, the
    -- index is adjusted for any scrolling which happened since the request
    -- started.
  , scrollPos:Int
  , scrollSpeed:Int
    -- List of HTTP requests that have been made for jedis. When a request
    -- completes, it is removed from the list. On scrolling, requests for
    -- out-of-view jedis are aborted and removed from this list.
  , jediRequests:List JediRequest
    -- List of requests that have been aborted and should be resumed later.
  , requestsToResume:List JediRequest
  , nextRequestId:Int
  }


type alias World =
  { id:Int
  , name:String
  }


type alias Jedi =
  { id:Int
  , name:String
  , homeworld:World
  , master:Maybe JediUrl
  , apprentice:Maybe JediUrl
  }


type alias JediUrl =
  { id:Int
  , url:String
  }


type alias JediRequest =
  { id:Int
  , jediUrl:JediUrl
  , insertPos:Int
  , scrollPos:Int
  , abort:Cmd Msg}


type ScrollDir
  = Up
  | Down


mkJediUrl : Int -> JediUrl
mkJediUrl id =
  { id=id
  , url="http://localhost:3000/dark-jedis/" ++ toString id
  }

darthSidious : JediUrl
darthSidious = mkJediUrl 3616


init : Int -> Int -> JediUrl -> (Model, Cmd Msg)
init nbSlots scrollSpeed jediUrl =
  fetchJedi 0 (nbSlots // 2) jediUrl (initModel nbSlots scrollSpeed)


initModel : Int -> Int -> Model
initModel nbSlots scrollSpeed =
  { world = Nothing
  , jediSlots = Array.repeat nbSlots Nothing
  , scrollPos = 0
  , scrollSpeed = scrollSpeed
  , jediRequests = []
  , requestsToResume = []
  , nextRequestId = 0
  }


--
-- Messages
--


type Msg
  = SetWorld String
  | SetJedi JediRequest
            (Result Http.Error Jedi)
  | Scroll ScrollDir Int
  | NoAction


--
-- Update
--


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    SetJedi request newJediResult ->
      setJedi request newJediResult model

    SetWorld response ->
      case decodeString decodeWorld response of
        Err _ ->
          pure model
        Ok world ->
          setWorld world model

    Scroll dir scrollSpeed ->
      doScroll model dir scrollSpeed

    NoAction ->
      pure model


--
-- Business logic
--

{-| Set Obi-Wan's current world.
If any of the dark jedis in view are on the same world, abort all outstanding
jedi requests.
-}
setWorld : World -> Model -> (Model, Cmd Msg)
setWorld world model =
  let model' = { model | world = Just world }
  in
      if any (flip onWorld (Just world)) model'.jediSlots
        then abortAndSaveAllRequests model'
        else resumeAllRequests model'


{-| Set the jedi at request.insertPos after adjusting for scrolling, remove the
completed request from the list, and fetch the jedis before/after the new jedi
if required.
If Obi-Wan is on the new jedi's homeworld, abort and save all outstanding requests.
-}
setJedi : JediRequest -> Result Http.Error Jedi -> Model -> (Model, Cmd Msg)
setJedi request newJediResult model =
  let newMJedi = Result.toMaybe newJediResult

      adjustedPos =
        adjustPos request.insertPos
                  request.scrollPos
                  model.scrollPos

      newJediSlots =
        if inBounds adjustedPos model.jediSlots
          then Array.set adjustedPos newMJedi model.jediSlots
          -- Don't update the model if this jedi has been scrolled off-screen.
          else model.jediSlots

      model' = { model | jediRequests = removeRequest request model.jediRequests
                       , jediSlots = newJediSlots }

      maybeRetry =
        case newJediResult of
          Err httpErr ->
            retryRequest 1000 request
          Ok _ ->
            pure

      maybeAbortAll =
        if (newMJedi `onWorld` model.world)
          then abortAndSaveAllRequests
          else pure

  in
      maybeFetchJedisAround adjustedPos model'
        >>= maybeRetry
        >>= maybeAbortAll


{-| Extract requests for jedis that are no longer in view and need to be
aborted.
-}
abortRequests : Model -> (Model, Cmd Msg)
abortRequests model =
  let (newRequests, requestsToAbort) =
        List.partition
          (\request ->
             (inBounds (adjustPos request.insertPos
                        request.scrollPos
                        model.scrollPos)
             model.jediSlots))
          model.jediRequests
      aborts = List.map .abort requestsToAbort
  in
      ( { model | jediRequests = newRequests }
      , Cmd.batch aborts )


{-| Abort all outstanding requests, and save them for resuming later.
-}
abortAndSaveAllRequests : Model -> (Model, Cmd Msg)
abortAndSaveAllRequests model =
  ( { model | jediRequests = []
            , requestsToResume = List.append model.requestsToResume model.jediRequests }
  , Cmd.batch (List.map .abort model.jediRequests) )


{-| Resume requests that have been aborted
-}
resumeAllRequests : Model -> (Model, Cmd Msg)
resumeAllRequests model =
  let model' =
        { model | requestsToResume = [] }
  in
      pure model' `bindAll`
        List.map (retryRequest 0) model.requestsToResume


{-|  Scrolling logic. If we can scroll (see `canScroll`):
* remove `scrollSpeed` jedis from the beginning (end) of the slots list,
* add `scrollSpeed` empty slots to the end (beginning),
* abort any requests for jedis that are now out of view, and
* if the first (last) jedi has a master (an apprentice), fire off a new jedi
  request.
-}
doScroll : Model -> ScrollDir -> Int -> (Model, Cmd Msg)
doScroll model dir scrollSpeed =
  if not (canScroll dir scrollSpeed model)
    then pure model
    else
      let slotsLength =
            Array.length model.jediSlots

          emptySlots =
            Array.repeat scrollSpeed Nothing

          (newJedis, newScrollPos, endJediPos) =
            case dir of
              Up ->
                ( Array.append emptySlots (Array.slice 0 (slotsLength - scrollSpeed) model.jediSlots)
                , model.scrollPos - scrollSpeed
                , scrollSpeed
                )
              Down ->
                ( Array.append (Array.slice scrollSpeed slotsLength model.jediSlots) emptySlots
                , model.scrollPos + scrollSpeed
                , slotsLength - scrollSpeed - 1
                )

      in
          pure { model | jediSlots = newJedis
                       , scrollPos = newScrollPos }
            >>= abortRequests
            >>= maybeFetchJedisAround endJediPos


fetchJedi : Float -> Int -> JediUrl -> Model -> (Model, Cmd Msg)
fetchJedi sleepMillis insertPos jediUrl model =
  let sendTask =
        Http.get decodeJedi jediUrl.url

      -- TODO: re-implement aborting requests
      abortEffect =
        Cmd.none
        -- abortTask
        --   |> Task.toMaybe
        --   |> Task.map (\_ -> NoAction)
        --   |> Cmd.task

      request =
        { id = model.nextRequestId
        , jediUrl = jediUrl
        , insertPos = insertPos
        , scrollPos = model.scrollPos
        , abort = abortEffect }

      sendEffect =
        Process.sleep sleepMillis
          `Task.andThen` (\_ -> sendTask)
            |> Task.toResult
            |> Task.perform (\_ -> NoAction) (SetJedi request)

  in
      ( { model | jediRequests = request :: model.jediRequests
                , nextRequestId = model.nextRequestId + 1 }
      , sendEffect )


{-| Replay a request (after adjusting the insert position)
-}
retryRequest : Float -> JediRequest -> Model -> (Model, Cmd Msg)
retryRequest sleepMillis request model =
  let model' = { model | jediRequests = removeRequest request model.jediRequests }
      newInsertPos =
        adjustPos request.insertPos
                  request.scrollPos
                  model'.scrollPos
  in
      fetchJedi sleepMillis newInsertPos request.jediUrl model'


{-| Check whether we have jedis around the jedi at `pos`, and fetch them if we
don't.
-}
maybeFetchJedisAround : Int -> Model -> (Model, Cmd Msg)
maybeFetchJedisAround pos model =
  pure model
    >>= maybeFetchJedi pos (pos - 1) .master
    >>= maybeFetchJedi pos (pos + 1) .apprentice


maybeFetchJedi : Int -> Int -> (Jedi -> Maybe JediUrl) -> Model -> (Model, Cmd Msg)
maybeFetchJedi pos nextPos getNextUrl model =
  let
    mNext =
      if needJediAt nextPos model
        then Array.get pos model.jediSlots `andThenAndThen` getNextUrl
        else Nothing
  in
    case mNext of
      Just nextUrl ->
        fetchJedi 0 nextPos nextUrl model
      Nothing ->
        pure model


--
-- Helpers
--


adjustPos : Int -> Int -> Int -> Int
adjustPos pos oldScrollPos newScrollPos =
  let offset = oldScrollPos - newScrollPos
  in pos + offset


removeRequest : JediRequest -> List JediRequest -> List JediRequest
removeRequest request requests =
  List.filter (\ r -> r.id /= request.id) requests


haveJediAt : Int -> Model -> Bool
haveJediAt pos {jediSlots} = Array.get pos jediSlots /= Just Nothing


needJediAt : Int -> Model -> Bool
needJediAt pos model =
  inBounds pos model.jediSlots && not (haveJediAt pos model)


{-| Return True if the first (last) jedi in the list has an apprentice (master)
AND we would have at least one jedi in view after the scroll.
-}
canScroll : ScrollDir -> Int -> Model -> Bool
canScroll upOrDown scrollSpeed model =
  let loadedJedis =
        Array.filter notNothing model.jediSlots

      (getFirstOrLast, apprenticeOrMaster, scrollStart, scrollEnd) =
        case upOrDown of
          Up ->
            ( aFirst
            , .master
            , 0
            , -scrollSpeed)
          Down ->
            ( aLast
            , .apprentice
            , scrollSpeed
            , Array.length model.jediSlots)

      next =
        getFirstOrLast loadedJedis
          `andThenAndThen` apprenticeOrMaster

      jediInView =
        model.jediSlots
          |> Array.slice scrollStart scrollEnd
          |> any notNothing

      scrollDisabled =
        any (flip onWorld model.world) model.jediSlots
  in
      notNothing next && jediInView && not scrollDisabled


onWorld : Maybe Jedi -> Maybe World -> Bool
onWorld mJedi mWorld =
  case mMap2 (,) mWorld mJedi of
    Just (world, jedi) ->
      jedi.homeworld.id == world.id
    Nothing -> False


--
-- Views
--


view : Model -> Html Msg
view model =
  div [ class "css-root" ]
    [ viewPlanetMonitor model.world
    , viewJediList model
    ]


viewPlanetMonitor : Maybe World -> Html Msg
viewPlanetMonitor mWorld =
  h1 [ class "css-planet-monitor" ]
    [ text ("Obi-Wan currently "
            ++
            (case mWorld of
               Just {name} -> "on " ++ name
               Nothing -> "in transit"))
    ]


viewJediList : Model -> Html Msg
viewJediList model =
    div [ class "css-scrollable-list" ]
      [ ul [ class "css-slots" ]
          (List.map (viewJedi model.world)
                    (Array.toList model.jediSlots))
      , viewScrollButtons model
      ]


viewJedi : Maybe World -> Maybe Jedi -> Html Msg
viewJedi mWorld mJedi =
  li
    [ class "css-slot"
    , style (if mJedi `onWorld` mWorld
               then [("color", "red")]
               else [])
    ]
    (case mJedi of
       Nothing -> []
       Just jedi ->
         [ h3 [] [ text jedi.name ]
         , h6 [] [ text jedi.homeworld.name ]
         ]
    )


viewScrollButtons : Model -> Html Msg
viewScrollButtons model =
  div [ class "css-scroll-buttons" ]
    (List.map
        (viewScrollButton model)
        [ Up, Down ])


viewScrollButton : Model -> ScrollDir -> Html Msg
viewScrollButton model dir =
  let className =
        case dir of
          Up ->
            "css-button-up"
          Down ->
            "css-button-down"

      enabled = canScroll dir model.scrollSpeed model

      classes = classList [ (className, True)
                          , ("css-button-disabled", not enabled)
                          ]

      clickHandler = onClick (Scroll dir model.scrollSpeed)
  in
      button
        [classes, clickHandler]
        []


--
-- Decoders
--


decodeJedi : Json.Decoder Jedi
decodeJedi =
  Json.object5 Jedi
    ("id" := Json.int)
    ("name" := Json.string)
    ("homeworld" := decodeWorld)
    ("master" := decodeJediUrl)
    ("apprentice" := decodeJediUrl)


decodeWorld : Json.Decoder World
decodeWorld =
  Json.object2 World
    ("id" := Json.int)
    ("name" := Json.string)


-- If id is null return Nothing, otherwise return a JediUrl
decodeJediUrl : Json.Decoder (Maybe JediUrl)
decodeJediUrl =
  ("id" :=
     (Json.oneOf
        [ Json.map Just Json.int
        , Json.null Nothing
        ]))
  `Json.andThen`
    (\mId ->
       case mId of
         Just id ->
           Json.object1 (\url -> Just (JediUrl id url))
             ("url" := Json.string)
         Nothing ->
           -- id was null, return Nothing
           Json.succeed Nothing)


--
-- Lib
--


inBounds : Int -> Array x -> Bool
inBounds pos slots =
  pos >= 0 && pos < Array.length slots


notNothing : Maybe x -> Bool
notNothing maybe =
  case maybe of
    Nothing -> False
    Just _  -> True


isNothing : Maybe x -> Bool
isNothing = not << notNothing


{-| Naive Array.any
 -}
any : (a -> Bool) -> Array a -> Bool
any pred array = Array.length (Array.filter pred array) > 0


{-| Maybe.map2 from elm-lang/core 3.0.0
 -}
mMap2 : (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
mMap2 func ma mb =
  case (ma,mb) of
    (Just a, Just b) -> Just (func a b)
    _ -> Nothing


{-| The same as Maybe.andThen, but when your input is a nested Maybe.
 -}
andThenAndThen : Maybe (Maybe a) -> (a -> Maybe b) -> Maybe b
andThenAndThen mmValue f =
  mmValue `andThen` flip andThen f


{-| Monadic pure: lift a Model to a (Model, Cmd Msg).
-}
pure : a -> (a, Cmd b)
pure model = (model, Cmd.none)


{-| Monadic bind: compose effectful computations.
-}
(>>=) : (a, Cmd b) -> (a -> (c, Cmd b)) -> (c, Cmd b)
(>>=) (model, effects) f =
  let (model', effects') = f model
  in
      (model', Cmd.batch [effects, effects'])


{-| Does this have a well known name?
Thread a (model, effects) pair through a list of (model -> (model, effects))
functions.
-}
bindAll : (a, Cmd b) -> List (a -> (a, Cmd b)) -> (a, Cmd b)
bindAll modelEffects fs =
  List.foldl (flip (>>=)) modelEffects fs


aFirst : Array a -> Maybe a
aFirst = Array.get 0


aLast : Array a -> Maybe a
aLast arr = Array.get (Array.length arr - 1) arr
