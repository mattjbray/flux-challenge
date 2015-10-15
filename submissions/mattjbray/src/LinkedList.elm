module LinkedList ( Model
                  , Config
                  , init
                  , Action(SetContext)
                  , update
                  , view ) where

import Array exposing (Array)
import Effects exposing (Effects)
import Html exposing (Html, button, div, h1, h3, h6, li, text, ul)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import Json.Decode as Json
import Http
import Task

import Lib exposing ( (>>=)
                    , aFirst
                    , aLast
                    , andThenAndThen
                    , any
                    , bindAll
                    , inBounds
                    , isNothing
                    , mMap2
                    , notNothing
                    , pure )


--
-- Models
--


type alias Model item context =
  { context:context
    -- Condition under which all requests will be aborted.
    -- When the condition evaluates to false again, requests are resumed.
  , suspendCondition:context -> Array (Maybe item) -> Bool
  , getNextUrl:item -> Maybe String
  , getPrevUrl:item -> Maybe String
  , decodeItem:Json.Decoder item
  , viewItem:context -> item -> Html
    -- The slots in view, which may contain items
  , slots:Array (Maybe item)
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
  , pendingRequests:List (Request item context)
    -- List of requests that have been aborted and should be resumed later.
  , suspendedRequests:List (Request item context)
  , nextRequestId:Int
  }


type alias Config item context =
  { initContext:context
  , initUrl:String
  , suspendCondition:context -> Array (Maybe item) -> Bool
  , getNextUrl:item -> Maybe String
  , getPrevUrl:item -> Maybe String
  , decodeItem:Json.Decoder item
  , viewItem:context -> item -> Html
  , nbSlots:Int
  , scrollSpeed:Int
  }


type alias Request item context =
  { id:Int
  , url:String
  , insertPos:Int
  , scrollPos:Int
  , abort:Effects (Action item context)}


type ScrollDir
  = Up
  | Down


init : Config item context -> (Model item context, Effects (Action item context))
init config =
  let model = initModel config
  in
      fetchItem 0 (config.nbSlots // 2) config.initUrl model


initModel : Config item context -> Model item context
initModel config =
  { context = config.initContext
  , suspendCondition = config.suspendCondition
  , getNextUrl = config.getNextUrl
  , getPrevUrl = config.getPrevUrl
  , decodeItem = config.decodeItem
  , viewItem = config.viewItem
  , scrollSpeed = config.scrollSpeed
  , slots = Array.repeat config.nbSlots Nothing
  , scrollPos = 0
  , pendingRequests = []
  , suspendedRequests = []
  , nextRequestId = 0
  }


--
-- Actions
--


type Action item context
  = SetContext context
  | SetItem (Request item context)
            (Result Http.Error item)
  | Scroll ScrollDir Int
  | NoAction


--
-- Update
--


update : (Action item context) -> Model item context -> (Model item context, Effects (Action item context))
update action model =
  case action of
    SetItem request newItemResult ->
      setItem request newItemResult model

    SetContext context ->
      setContext context model

    Scroll dir scrollSpeed ->
      doScroll model dir scrollSpeed

    NoAction ->
      pure model


--
-- Business logic
--

{-| Set the current context.
If suspendCondition evaluates to true, abort all requests. If it evaluates to
false, resume all aborted requests.
-}
setContext : context -> Model item context -> (Model item context, Effects (Action item context))
setContext context model =
  let model' = { model | context <- context }
  in
      if suspendCondition model'
        then abortAndSaveAllRequests model'
        else resumeAllRequests model'


{-| Set the item at request.insertPos after adjusting for scrolling, remove the
completed request from the list, and fetch the items before/after the new item
if required.
If suspendCondition evaluates to true, abort all requests.
-}
setItem : (Request item context)
        -> Result Http.Error item
        -> Model item context
        -> (Model item context, Effects (Action item context))
setItem request newItemResult model =
  let newMItem = Result.toMaybe newItemResult

      adjustedPos =
        adjustPos request.insertPos
                  request.scrollPos
                  model.scrollPos

      newSlots =
        if inBounds adjustedPos model.slots
          then Array.set adjustedPos newMItem model.slots
          -- Don't update the model if this jedi has been scrolled off-screen.
          else model.slots

      model' = { model | pendingRequests <- removeRequest request model.pendingRequests
                       , slots <- newSlots }

      maybeRetry =
        case newItemResult of
          Err httpErr ->
            retryRequest 1000 request
          Ok _ ->
            pure

      maybeAbortAll =
        if suspendCondition model'
          then abortAndSaveAllRequests
          else pure

  in
      maybeFetchItemsAround adjustedPos model'
        >>= maybeRetry
        >>= maybeAbortAll


{-| Extract requests for jedis that are no longer in view and need to be
aborted.
-}
abortRequests : Model item context -> (Model item context, Effects (Action item context))
abortRequests model =
  let (newRequests, requestsToAbort) =
        List.partition
          (\request ->
             (inBounds (adjustPos request.insertPos
                        request.scrollPos
                        model.scrollPos)
             model.slots))
          model.pendingRequests
      aborts = List.map .abort requestsToAbort
  in
      ( { model | pendingRequests <- newRequests }
      , Effects.batch aborts )


{-| Abort all outstanding requests, and save them for resuming later.
-}
abortAndSaveAllRequests : Model item context -> (Model item context, Effects (Action item context))
abortAndSaveAllRequests model =
  ( { model | pendingRequests <- []
            , suspendedRequests <- List.append model.suspendedRequests model.pendingRequests }
  , Effects.batch (List.map .abort model.pendingRequests) )


{-| Resume requests that have been aborted
-}
resumeAllRequests : Model item context -> (Model item context, Effects (Action item context))
resumeAllRequests model =
  let model' =
        { model | suspendedRequests <- [] }
  in
      pure model' `bindAll`
        List.map (retryRequest 0) model.suspendedRequests


{-|  Scrolling logic. If we can scroll (see `canScroll`):
* remove `scrollSpeed` items from the beginning (end) of the slots list,
* add `scrollSpeed` empty slots to the end (beginning),
* abort any requests for slots that are now out of view, and
* if the first (last) slot has a prevUrl (a nextUrl), fire off a new request.
-}
doScroll : Model item context -> ScrollDir -> Int -> (Model item context, Effects (Action item context))
doScroll model dir scrollSpeed =
  if not (canScroll dir scrollSpeed model)
    then pure model
    else
      let slotsLength =
            Array.length model.slots

          emptySlots =
            Array.repeat scrollSpeed Nothing

          (newSlots, newScrollPos, endItemPos) =
            case dir of
              Up ->
                ( Array.append emptySlots (Array.slice 0 (slotsLength - scrollSpeed) model.slots)
                , model.scrollPos - scrollSpeed
                , scrollSpeed
                )
              Down ->
                ( Array.append (Array.slice scrollSpeed slotsLength model.slots) emptySlots
                , model.scrollPos + scrollSpeed
                , slotsLength - scrollSpeed - 1
                )

      in
          pure { model | slots <- newSlots
                       , scrollPos <- newScrollPos }
            >>= abortRequests
            >>= maybeFetchItemsAround endItemPos


fetchItem : Float -> Int -> String -> Model item context -> (Model item context, Effects (Action item context))
fetchItem sleepMillis insertPos url model =
  let (sendTask, abortTask) =
        Http.getWithAbort model.decodeItem url

      abortEffect =
        abortTask
          |> Task.toMaybe
          |> Task.map (\_ -> NoAction)
          |> Effects.task

      request =
        { id = model.nextRequestId
        , url = url
        , insertPos = insertPos
        , scrollPos = model.scrollPos
        , abort = abortEffect }

      sendEffect =
        Task.sleep sleepMillis
          `Task.andThen` (\_ -> sendTask)
            |> Task.toResult
            |> Task.map (SetItem request)
            |> Effects.task

  in
      ( { model | pendingRequests <- request :: model.pendingRequests
                , nextRequestId <- model.nextRequestId + 1 }
      , sendEffect )


{-| Replay a request (after adjusting the insert position)
-}
retryRequest : Float -> (Request item context) -> Model item context -> (Model item context, Effects (Action item context))
retryRequest sleepMillis request model =
  let model' = { model | pendingRequests <- removeRequest request model.pendingRequests }
      newInsertPos =
        adjustPos request.insertPos
                  request.scrollPos
                  model'.scrollPos
  in
      fetchItem sleepMillis newInsertPos request.url model'


{-| Check whether we have items around the items at `pos`, and fetch them if we
don't.
-}
maybeFetchItemsAround : Int -> Model item context -> (Model item context, Effects (Action item context))
maybeFetchItemsAround pos model =
  pure model
    >>= maybeFetchItemAt pos (pos - 1) model.getPrevUrl
    >>= maybeFetchItemAt pos (pos + 1) model.getNextUrl


maybeFetchItemAt : Int -> Int -> (item -> Maybe String) -> Model item context -> (Model item context, Effects (Action item context))
maybeFetchItemAt pos nextPos getNextUrl model =
  let
    mNextUrl =
      if needItemAt nextPos model
        then Array.get pos model.slots `andThenAndThen` getNextUrl
        else Nothing
  in
    case mNextUrl of
      Just nextUrl ->
        fetchItem 0 nextPos nextUrl model
      Nothing ->
        pure model


--
-- Helpers
--


suspendCondition : Model item context -> Bool
suspendCondition model = model.suspendCondition model.context model.slots


adjustPos : Int -> Int -> Int -> Int
adjustPos pos oldScrollPos newScrollPos =
  let offset = oldScrollPos - newScrollPos
  in pos + offset


removeRequest : (Request item context) -> List (Request item context) -> List (Request item context)
removeRequest request requests =
  List.filter (\ r -> r /= request) requests


haveItemAt : Int -> Model item context -> Bool
haveItemAt pos {slots} = Array.get pos slots /= Just Nothing


needItemAt : Int -> Model item context -> Bool
needItemAt pos model =
  inBounds pos model.slots && not (haveItemAt pos model)


{-| Return True if the first (last) item in the list has a prev url (next url)
AND we would have at least one item in view after the scroll.
-}
canScroll : ScrollDir -> Int -> Model item context -> Bool
canScroll upOrDown scrollSpeed model =
  let loadedItems =
        Array.filter notNothing model.slots

      (getFirstOrLast, nextOrPrev, scrollStart, scrollEnd) =
        case upOrDown of
          Up ->
            ( aFirst
            , model.getPrevUrl
            , 0
            , -scrollSpeed)
          Down ->
            ( aLast
            , model.getNextUrl
            , scrollSpeed
            , Array.length model.slots)

      next =
        getFirstOrLast loadedItems
          `andThenAndThen` nextOrPrev

      itemInView =
        model.slots
          |> Array.slice scrollStart scrollEnd
          |> any notNothing

  in
      notNothing next && itemInView


--
-- Views
--


view : Signal.Address (Action item context) -> Model item context -> Html
view address model =
    div [ class "css-scrollable-list" ]
      [ ul [ class "css-slots" ]
          (List.map (viewItem model)
                    (Array.toList model.slots))
      , viewScrollButtons address model
      ]

viewItem : Model item context -> Maybe item -> Html
viewItem model mItem =
  li
    [ class "css-slot" ]
    (case mItem of
       Just item ->
         [model.viewItem model.context item]
       Nothing ->
         [])


viewScrollButtons : Signal.Address (Action item context) -> Model item context -> Html
viewScrollButtons address model =
  let scrollDisabled = suspendCondition model
  in
    div [ class "css-scroll-buttons" ]
      (List.map
         (viewScrollButton address scrollDisabled model model.scrollSpeed)
         [ Up, Down ])


viewScrollButton : Signal.Address (Action item context) -> Bool -> Model item context -> Int -> ScrollDir -> Html
viewScrollButton address scrollDisabled model scrollSpeed dir =
  let className =
        case dir of
          Up ->
            "css-button-up"
          Down ->
            "css-button-down"

      enabled = not scrollDisabled && canScroll dir scrollSpeed model

      classes = classList [ (className, True)
                          , ("css-button-disabled", not enabled)
                          ]

      clickHandler = onClick address (Scroll dir scrollSpeed)
  in
      button
        (if enabled
           then [classes, clickHandler]
           else [classes])
        []
