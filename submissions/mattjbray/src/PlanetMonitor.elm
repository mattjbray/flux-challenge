module PlanetMonitor ( Model
                     , init
                     , Action(SetWorld)
                     , update
                     , view ) where

import Effects exposing (Effects)
import Html exposing (Html, h1, text)
import Html.Attributes exposing (class)

import Lib exposing (pure)
import World exposing (World)


type alias Model = Maybe World


init : (Model, Effects Action)
init = (Nothing, Effects.none)


type Action = SetWorld (Maybe World)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SetWorld mWorld ->
      pure mWorld


view : Signal.Address Action -> Model -> Html
view address model =
  h1 [ class "css-planet-monitor" ]
     [ text ("Obi-Wan currently "
             ++
             (case model of
                Just {name} -> "on " ++ name
                Nothing -> "in transit")) ]
