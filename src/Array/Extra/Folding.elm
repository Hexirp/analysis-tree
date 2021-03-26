module Array.Extra.Folding exposing (maximum, minimum)

{-| 配列についての便利な折り畳み系の関数たちです。 -}

import Basics exposing (..)

import Maybe exposing (Maybe)

import Array exposing (..)

{-| 非空の配列から最大元を見つけます。 -}
maximum : Array comparable -> Maybe comparable
maximum array = Maybe.map (\e -> foldl max e array) (get 0 array)

{-| 非空の配列から最小元を見つけます。 -}
minimum : Array comparable -> Maybe comparable
minimum array = Maybe.map (\e -> foldl min e array) (get 0 array)
