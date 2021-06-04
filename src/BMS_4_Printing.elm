module BMS_4_Printing
  exposing
    (
      printRawMatrix
    ,
      printMatrix
    )

import Tuple
import Array exposing (Array)
import String

import BMS_4

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
