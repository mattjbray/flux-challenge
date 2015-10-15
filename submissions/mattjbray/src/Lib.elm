module Lib where

import Array exposing (Array)
import Effects exposing (Effects)
import Maybe exposing (andThen)


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


{-| Monadic pure: lift a Model to a (Model, Effects Action).
-}
pure : a -> (a, Effects b)
pure model = (model, Effects.none)


{-| Monadic bind: compose effectful computations.
-}
(>>=) : (a, Effects b) -> (a -> (c, Effects b)) -> (c, Effects b)
(model, effects) >>= f =
  let (model', effects') = f model
  in
      (model', Effects.batch [effects, effects'])


{-| Does this have a well known name?
Thread a (model, effects) pair through a list of (model -> (model, effects))
functions.
-}
bindAll : (a, Effects b) -> List (a -> (a, Effects b)) -> (a, Effects b)
bindAll modelEffects fs =
  List.foldl (flip (>>=)) modelEffects fs


aFirst : Array a -> Maybe a
aFirst = Array.get 0


aLast : Array a -> Maybe a
aLast arr = Array.get (Array.length arr - 1) arr
