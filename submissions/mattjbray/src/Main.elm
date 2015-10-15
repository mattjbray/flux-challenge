module Main where

import Effects exposing (Effects)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import StartApp
import Task exposing (Task)

import World exposing (World)
import PlanetMonitor
import JediList


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs =
        [ Signal.map (PlanetMonitorAction << PlanetMonitor.SetWorld) currentWorld
        , Signal.map (JediListAction << JediList.SetWorld) currentWorld ]
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
  , jediList:JediList.Model
  }


init : (Model, Effects Action)
init =
  let nbSlots = 5
      scrollSpeed = 2
      initialJediId = 3616

      (planetMonitor, pmEffects) =
        PlanetMonitor.init

      (jediList, jlEffects) =
        JediList.init nbSlots scrollSpeed initialJediId
  in
    ( { planetMonitor = planetMonitor
      , jediList = jediList }
    , Effects.batch
        [ Effects.map JediListAction jlEffects
        , Effects.map PlanetMonitorAction pmEffects ] )


type Action
  = PlanetMonitorAction PlanetMonitor.Action
  | JediListAction JediList.Action


{-| Dispatch actions to components
-}
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    PlanetMonitorAction act ->
      let (planetMonitor, effects) =
            PlanetMonitor.update act model.planetMonitor
      in
          ( { model | planetMonitor <- planetMonitor }
          , Effects.map PlanetMonitorAction effects )

    JediListAction act ->
      let (jediList, effects) =
            JediList.update act model.jediList
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
    , JediList.view
        (Signal.forwardTo address JediListAction)
        model.jediList ]
