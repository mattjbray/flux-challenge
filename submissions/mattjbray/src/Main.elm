module Main where

import Effects exposing (Effects)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import StartApp
import Task exposing (Task)

import Lib exposing ((>>=), pure)
import World exposing (World)
import PlanetMonitor
import LinkedList
import Jedi


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [ Signal.map SetWorld currentWorld ]
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks


{-| index.html creates a websocket and calls this port whenever a message is
received.
-}
port currentWorld : Signal (Maybe World)


type alias Model =
  { planetMonitor:PlanetMonitor.Model
  , jediList:LinkedList.Model Jedi.Jedi (Maybe World)
  }


{-| Wire up our LinkedList with Jedis -}
listConfig : LinkedList.Config Jedi.Jedi (Maybe World)
listConfig =
  { initContext = Nothing
  , initUrl = (Jedi.mkJediUrl 3616).url
  , suspendCondition = Jedi.anyOnWorld
  , getNextUrl = Jedi.getNextUrl
  , getPrevUrl = Jedi.getPrevUrl
  , decodeItem = Jedi.decodeJedi
  , viewItem = Jedi.view
  , nbSlots = 5
  , scrollSpeed = 2
  }

init : (Model, Effects Action)
init =
  let (planetMonitor, pmEffects) =
        PlanetMonitor.init

      (jediList, jlEffects) =
        LinkedList.init listConfig
  in
    ( { planetMonitor = planetMonitor
      , jediList = jediList }
    , Effects.batch
        [ Effects.map JediListAction jlEffects
        , Effects.map PlanetMonitorAction pmEffects ] )


type Action
  = SetWorld (Maybe World)
  | PlanetMonitorAction PlanetMonitor.Action
  | JediListAction (LinkedList.Action Jedi.Jedi (Maybe World))


{-| Dispatch actions to components
-}
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SetWorld mWorld ->
      pure model
        >>= update (PlanetMonitorAction (PlanetMonitor.SetWorld mWorld))
        >>= update (JediListAction (LinkedList.SetContext mWorld))

    PlanetMonitorAction act ->
      let (planetMonitor, effects) =
            PlanetMonitor.update act model.planetMonitor
      in
          ( { model | planetMonitor <- planetMonitor }
          , Effects.map PlanetMonitorAction effects )

    JediListAction act ->
      let (jediList, effects) =
            LinkedList.update act model.jediList
      in
          ( { model | jediList <- jediList }
          , Effects.map JediListAction effects )


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "css-root" ]
    [ PlanetMonitor.view
        (Signal.forwardTo address PlanetMonitorAction)
        model.planetMonitor
    , LinkedList.view
        (Signal.forwardTo address JediListAction)
        model.jediList ]
