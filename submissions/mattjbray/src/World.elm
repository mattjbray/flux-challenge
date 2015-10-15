module World (World, decodeWorld) where

import Json.Decode as Json exposing ((:=))


type alias World =
  { id:Int
  , name:String
  }


decodeWorld : Json.Decoder World
decodeWorld =
  Json.object2 World
    ("id" := Json.int)
    ("name" := Json.string)
