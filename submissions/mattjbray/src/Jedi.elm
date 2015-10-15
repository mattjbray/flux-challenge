module Jedi ( Jedi
            , mkJediUrl
            , anyOnWorld
            , getNextUrl
            , getPrevUrl
            , decodeJedi
            , view) where

import Array exposing (Array)
import Html exposing (Html, div, h3, h6, text)
import Html.Attributes exposing (style)
import Json.Decode as Json exposing ((:=))

import Lib exposing (any, mMap2)
import World exposing (World, decodeWorld)


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


mkJediUrl : Int -> JediUrl
mkJediUrl id =
  { id=id
  , url="http://localhost:3000/dark-jedis/" ++ toString id
  }


getNextUrl : Jedi -> Maybe String
getNextUrl jedi = Maybe.map .url jedi.apprentice


getPrevUrl : Jedi -> Maybe String
getPrevUrl jedi = Maybe.map .url jedi.master


onWorld : Maybe Jedi -> Maybe World -> Bool
onWorld mJedi mWorld =
  case mMap2 (,) mWorld mJedi of
    Just (world, jedi) ->
      jedi.homeworld.id == world.id
    Nothing -> False


anyOnWorld : Maybe World -> Array (Maybe Jedi) -> Bool
anyOnWorld mWorld jedis =
  any (flip onWorld mWorld) jedis


decodeJedi : Json.Decoder Jedi
decodeJedi =
  Json.object5 Jedi
    ("id" := Json.int)
    ("name" := Json.string)
    ("homeworld" := decodeWorld)
    ("master" := decodeJediUrl)
    ("apprentice" := decodeJediUrl)


-- If id is null return Nothing, otherwise return a JediUrl
decodeJediUrl : Json.Decoder (Maybe JediUrl)
decodeJediUrl =
  ("id" :=
     (Json.oneOf
        [ Json.map Just Json.int
        , Json.null Nothing ]))
  `Json.andThen`
    (\mId ->
       case mId of
         Just id ->
           Json.object1 (\url -> Just (JediUrl id url))
                 ("url" := Json.string)
         Nothing ->
           -- id was null, return Nothing
           Json.succeed Nothing)


view : Maybe World -> Jedi -> Html
view mWorld jedi =
  div
    [ style (if Just jedi `onWorld` mWorld
               then [("color", "red")]
               else []) ]
    [ h3 [] [ text jedi.name ]
    , h6 [] [ text jedi.homeworld.name ] ]

