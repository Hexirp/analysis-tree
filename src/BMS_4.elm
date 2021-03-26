module BMS_4
  exposing
    (
      fromArrayToList,
      fromListToArray,
      Nat,
      Matrix,
      fromMatrixToArray,
      fromArrayToMatrix,
      fromArrayToMatrixRawly,
      fromMatrixToList,
      fromListToMatrix,
      fromListToMatrixRawly,
      expand,
      Pindex,
      Patrix,
      fromMatrixToPatrix
    )

import Basics exposing (..)

import Maybe

import List

import Array exposing (Array)
import Array.Extra.Folding as Array

{-| 或る配列を或るリストへ変換します。 -}
fromArrayToList : Array (Array Int) -> List (List Int)
fromArrayToList array = Array.toList (Array.map Array.toList array)

{-| 或るリストを或る配列へ変換します。 -}
fromListToArray : List (List Int) -> Array (Array Int)
fromListToArray list = Array.map Array.fromList (Array.fromList list)

{-| これは自然数です。 -}
type Nat = Nat Int

{-| これはバシク行列システムにおける行列です。 -}
type Matrix = Matrix Int Int (Array (Array Int))

{-| 或る行列を或る配列へ変換します。 -}
fromMatrixToArray : Matrix -> Array (Array Int)
fromMatrixToArray matrix
  =
    case matrix of
      Matrix x y array -> array

{-| 或る配列を或る行列へ変換します。

其の配列が行列を正しく表していれば、そのようになります。もし、列の長さが不揃いであれば、最も長い列が行数の基準となり、それに合わせて他の列が底値でもってパディングされます。もし、行の長さがゼロであれば、行数（列の長さ）は不定値になりますので、デフォルトの値としてゼロとなります。

底値は、其の配列に含まれる最小の値です。ゼロを使わないのは、其の配列が表現する行列のトポロジーを可能な限り保つためです。
-}
fromArrayToMatrix : Array (Array Int) -> Matrix
fromArrayToMatrix x_y_array
  =
    let
      x = Array.length x_y_array
      y = Maybe.withDefault 0 (Array.maximum (Array.map Array.length x_y_array))
      e
        =
          Maybe.withDefault 0
            (Array.minimum
              (Array.map
                (\y_list -> Maybe.withDefault 0 (Array.minimum y_list))
                x_y_array))
      a = Array.map (fromArrayToMatrix_helper_1 y e) x_y_array
    in
      Matrix x y a

fromArrayToMatrix_helper_1 : Int -> Int -> Array Int -> Array Int
fromArrayToMatrix_helper_1 y e y_list
  = Array.initialize y (\i -> Maybe.withDefault e (Array.get i y_list))

{-| 或る行列を或るリストへと変換します。 -}
fromMatrixToList : Matrix -> List (List Int)
fromMatrixToList matrix
  =
    case matrix of
      Matrix x y x_y_array -> fromArrayToList x_y_array

{-| 或る配列を或る行列へ生のまま変換します。 -}
fromArrayToMatrixRawly : Int -> Int -> Array (Array Int) -> Matrix
fromArrayToMatrixRawly x y x_y_array = Matrix x y x_y_array

{-| 或るリストを或る行列へと変換します。

其のリストが行列を正しく表していれば、そのようになります。もし、列の長さが不揃いであれば、最も長い列が行数の基準となり、それに合わせて他の列が底値でもってパディングされます。もし、行の長さがゼロであれば、行数（列の長さ）は不定値になりますので、デフォルトの値としてゼロとなります。

底値は、其のリストに含まれる最小の値です。ゼロを使わないのは、其のリストが表現する行列のトポロジーを可能な限り保つためです。
-}
fromListToMatrix : List (List Int) -> Matrix
fromListToMatrix x_y_list
  =
    let
      x = List.length x_y_list
      y = Maybe.withDefault 0 (List.maximum (List.map List.length x_y_list))
      e
        =
          Maybe.withDefault 0
            (List.minimum
              (List.map
                (\y_list -> Maybe.withDefault 0 (List.minimum y_list))
                x_y_list))
      a = Array.fromList (List.map (fromListToMatrix_helper_1 y e) x_y_list)
    in
      Matrix x y a

fromListToMatrix_helper_1 : Int -> Int -> List Int -> Array Int
fromListToMatrix_helper_1 y e y_list
  = Array.initialize y (fromListToMatrix_helper_2 e y_list)

-- fromListToMatrix_helper_1 により i >= 0 である。
-- list-extra 8.3.0 の getAt で実装することも出来るが、上記の条件を使って実装を単純にしている。
fromListToMatrix_helper_2 : Int -> List Int -> Int -> Int
fromListToMatrix_helper_2 e y_list i
  =
    case y_list of
      [] -> e
      y_list_el :: y_list_
        ->
          if i == 0
            then y_list_el
            else fromListToMatrix_helper_2 e y_list_ (i - 1)

{-| 或るリストを生のまま行列へ変換します。 -}
fromListToMatrixRawly : Int -> Int -> List (List Int) -> Matrix
fromListToMatrixRawly x y x_y_list = Matrix x y (fromListToArray x_y_list)

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
