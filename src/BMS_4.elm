module BMS_4 exposing (Matrix, expand)

import Basics exposing (..)

import List exposing (List)

import Array exposing (Array)

{-| これは自然数です。 -}
type Nat = Nat Int

{-| これはバシク行列システムにおける行列です。 -}
type Matrix = Matrix Int Int (Array (Array Int))

{-| 或るリストを或る行列へと変換します。 -}
fromListToMatrix : List (List Int) -> Matrix
fromListToMatrix x_y_list
  =
    let x = List.length x_y_list in
      case fromListToMatrix_helper x_y_list of
        (y, generator) -> Matrix x y (generator y)

fromListToMatrix_helper : List (List Int) -> (Int, Int -> Array (Array Int))
fromListToMatrix_helper x_y_list
  =
    case x_y_list of
      [] -> (0, \n -> Array.empty)

{-| 或る行列を或る自然数により展開します。 `Just` で包んだ結果を返します。其の自然数が其の行列の共終タイプ以上なら `Nothing` を返します。 -}
expand : Matrix -> Nat -> Maybe Matrix
expand n x = expand n x

{-| これはピンデックスです。ピンデックスは或る行列の要素へのポインターを意味します。 -}
type Pindex = Null | Pindex Int

{-| これはパトリックスです。パトリックスはピンデックスの行列を意味します。 -}
type Patrix = Potrix Int Int (Array (Array Pindex))

{-| 或る行列をパトリックスへ変換します。 -}
fromMatrixToPatrix : Matrix -> Patrix
fromMatrixToPatrix x = fromMatrixToPatrix x
