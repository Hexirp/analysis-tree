module BMS_4 exposing (Matrix, expand)

import Basics exposing (..)

import Array exposing (Array)

{-| これは自然数です。 -}
type Nat = Nat Int

{-| これはバシク行列システムにおける行列です。 -}
type Matrix = Matrix Int Int (Array (Array Int))

{-| 或る行列を或る自然数により展開します。 `Just` で包んだ結果を返します。其の自然数が其の行列の共終タイプ以上なら `Nothing` を返します。 -}
expand : Matrix -> Nat -> Maybe Matrix
expand n x = expand n x
