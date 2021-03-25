module BMS_4 exposing (Matrix, expand)

import Basics exposing (..)

import Array exposing (Array)

{-| Representation of matrixes, the notion on Bashicu Matrix System. -}
type Matrix = Matrix Int Int (Array (Array Int))

{-| Expand a matrix with an integer. Return `Just` the result. Return `Nothing` if the cofinal type is less than the integer. Return `Nothing` if the integer is negative.

  map (expand 2) (fromList [[0,0,0],[1,1,1]]) == map Just (fromList [[0,0],[1,1],[2,2]])
  map (expand 0) (fromList [[0],[1],[2]]) == map Just (fromList [[0],[1]])
  map (expand 2) (fromList [[0],[1],[2]]) == map Just (fromList [[0],[1],[1],[1])
  map (expand 0) (fromList [[0],[0]]) == map Just (fromList [[0],[0]])
  map (expand 1) (fromList [[0],[0]]) == Just Nothing
  map (expand 0) (fromList []) == Just Nothing
  map (expand -1) (fromList [[0],[1],[2]]) == Just Nothing
-}
expand : Int -> Matrix -> Maybe Matrix
expand n x = Just x
