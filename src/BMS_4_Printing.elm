module BMS_4_Printing
  exposing
    (
      printRawMatrix
    ,
      printMatrix
    ,
      notation
    )

import Tuple

import Array exposing (Array)

import String

import Notation exposing (Maxipointed (..))
import BMS_4 exposing (Matrix)

import Notation_Printing exposing (NotationPrintable)

printRawMatrix : BMS_4.RawMatrix -> String
printRawMatrix x_y_int = Array.foldl (++) "" (Array.map printRawMatrix_helper_1 x_y_int)

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

printMatrix : BMS_4.Matrix -> String
printMatrix matrix = printRawMatrix (BMS_4.toRawMatrixFromMatrix matrix)

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
