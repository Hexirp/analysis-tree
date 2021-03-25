module BMS_4 exposing (Matrix, expand)

import Basics exposing (..)

import Maybe

import List

import Array exposing (Array)

{-| これは自然数です。 -}
type Nat = Nat Int

{-| これはバシク行列システムにおける行列です。 -}
type Matrix = Matrix Int Int (Array (Array Int))

{-| 或るリストを或る行列へと変換します。 -}
fromListToMatrix : List (List Int) -> Matrix
fromListToMatrix x_y_list
  =
    case fromListToMatrix_helper_1 x_y_list of
      (x, y, gen) -> Matrix x y (gen y)

fromListToMatrix_helper_1
  : List (List Int) -> (Int, Int, Int -> Array (Array Int))
fromListToMatrix_helper_1 x_y_list
  =
    case x_y_list of
      [] -> (0, 0, fromListToMatrix_helper_2)
      y_list :: x_y_list_ ->
        case fromListToMatrix_helper_1 x_y_list_ of
          (x_, y_, gen_) ->
            let
              x = x_ + 1
              y = max y_ (List.length y_list)
              gen = fromListToMatrix_helper_3 y_list x gen_
            in
              (x, y, gen)

fromListToMatrix_helper_2 : Int -> Array (Array Int)
fromListToMatrix_helper_2 = \n -> Array.empty

fromListToMatrix_helper_3
  : List Int -> Int -> (Int -> Array (Array Int)) -> Int -> Array (Array Int)
fromListToMatrix_helper_3 y_list x gen_
  =
    \n ->
      Array.initialize
        x
        (\i ->
          if i == 0
            then fromListToMatrix_helper_4 y_list n
            else Maybe.withDefault Array.empty (Array.get (i - 1) (gen_ n)))

fromListToMatrix_helper_4 : List Int -> Int -> Array Int
fromListToMatrix_helper_4 y_list n
  = Array.initialize n (\i -> fromListToMatrix_helper_5 y_list i)

fromListToMatrix_helper_5 : List Int -> Int -> Int
fromListToMatrix_helper_5 y_list i -- i > 0, from fromListToMatrix_helper_4.
  =
    case y_list of
      [] -> 0
      y_list_el :: y_list_ ->
        if i == 0
          then y_list_el
          else fromListToMatrix_helper_5 y_list_ (i - 1)

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
