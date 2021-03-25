module BMS_4 exposing (Matrix, expand)

import Basics exposing (..)

import Array exposing (Array)

{-| Representation of matrixes, the notion on Bashicu Matrix System. -}
type Matrix = Matrix Int Int (Array (Array Int))

{-| Expand a matrix with an integer. Return `Just` the result. Return `Nothing` if the cofinal type is less than the integer. Return `Nothing` if the integer is negative. -}
expand : Matrix -> Int -> Maybe Matrix
expand n x = expand n x
