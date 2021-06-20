module BMS_4_Printing
  exposing
    (
      printRawMatrix
    ,
      printMatrix
    ,
      notation
    )

{-| `BMS_4` 型に対して `NotationPrintable` 型を実装します。

# 定義
@docs printRawMatrix, printMatrix, notation
-}

import Tuple

import Array exposing (Array)

import String

import Notation exposing (Maxipointed (..))
import BMS_4 exposing (Matrix)

import Notation_Printing exposing (NotationPrintable)

{-| `RawMatrix` 型の print です。
-}
printRawMatrix : BMS_4.RawMatrix -> String
printRawMatrix x_y_int
  =
    let
      func x s = s ++ x
    in
      if Array.isEmpty x_y_int
        then "ε"
        else Array.foldl func "" (Array.map printRawMatrix_helper_1 x_y_int)

printRawMatrix_helper_1 : Array Int -> String
printRawMatrix_helper_1 y_int
  =
    let
      func int (string, bool)
        =
          if bool
            then (string ++ "," ++ String.fromInt int, True)
            else (string ++ String.fromInt int, True)
    in
      "(" ++ Tuple.first (Array.foldl func ("", False) y_int) ++ ")"

{-| `Matrix` 型の print です。
-}
printMatrix : BMS_4.Matrix -> String
printMatrix matrix = printRawMatrix (BMS_4.toRawMatrixFromMatrix matrix)

{-| `BMS_4` モジュールの `NotationPrintable` 型を実装します。
-}
notation : NotationPrintable (Maxipointed Matrix)
notation
  =
    {
      compare = BMS_4.notation.compare
    ,
      expand = BMS_4.notation.expand
    ,
      maximum = BMS_4.notation.maximum
    ,
      print
        =
          let
            func maxipointed_matrix
              =
                case maxipointed_matrix of
                  Lower matrix -> printMatrix matrix
                  Maximum -> "B"
          in
            func
    }
