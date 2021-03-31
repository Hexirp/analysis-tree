module BMS_4
  exposing
    (
      fromArrayToList,
      fromListToArray,
      Nat,
      Matrix,
      fromMatrixToArray,
      fromArrayToMatrix,
      fromMatrixToList,
      fromListToMatrix,
      expand,
      Pindex (..),
      Patrix,
      fromMatrixToPatrix,
      fromPatrixToMatrix,
      fromListToMatrixRawly,
      fromListToPatrixRawly
    )

import Case exposing (Case (..))

import Array exposing (Array)
import Array.Extra.Folding as Array

import BMS_4.Parsing as Parsing

import Debug

{-| 或る配列を或るリストへ変換します。 -}
fromArrayToList : Array (Array a) -> List (List a)
fromArrayToList array = Array.toList (Array.map Array.toList array)

{-| 或るリストを或る配列へ変換します。 -}
fromListToArray : List (List a) -> Array (Array a)
fromListToArray list = Array.map Array.fromList (Array.fromList list)

{-| これは自然数です。 -}
type Nat = Nat Int

{-| これはバシク行列システムにおける行列です。

`Matrix` 型の値は `verifyMatrix` を満たしていなければなりません。
-}
type Matrix = Matrix Int Int (Array (Array Int))

{-| 或る値が `Matrix` の規約を満たしているか検証します。 -}
verifyMatrix : Matrix -> Bool
verifyMatrix matrix
  =
    case matrix of
      Matrix x y x_y_array
        ->
          List.all
            (\a -> a)
            [
              Array.length x_y_array == x,
              Array.all (\a -> Array.length a == y) x_y_array
            ]

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

-- fromListToMatrix_helper_1 により 0 <= i である。
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

{-| 或る行列を或る自然数により展開します。 `Just` で包んだ結果を返します。其の自然数が其の行列の共終タイプ以上なら `Nothing` を返します。 -}
expand : Matrix -> Nat -> Maybe Matrix
expand n x = expand n x

{-| これはピンデックスです。ピンデックスは或る行列の要素へのポインターを意味します。 -}
type Pindex = Null | Pindex Int

{-| これはパトリックスです。パトリックスはピンデックスの行列を意味します。 -}
type Patrix = Patrix Int Int (Array (Array Pindex))

{-| 或る行列をパトリックスへ変換します。 -}
fromMatrixToPatrix : Matrix -> Case Patrix
fromMatrixToPatrix matrix
  =
    case matrix of
      Matrix x y x_y_int
        ->
          case
            Case.traverseArray
              (\case_x -> case_x)
              (Array.map
                (Case.traverseArray (\case_x -> case_x))
                (fromMatrixToPatrix_helper_1 x y x_y_int))
          of
            ImpossibleCase -> ImpossibleCase
            PossibleCase x_y_pindex -> PossibleCase (Patrix x y x_y_pindex)

fromMatrixToPatrix_helper_1
  : Int -> Int -> Array (Array Int) -> Array (Array (Case Pindex))
fromMatrixToPatrix_helper_1 x y x_y_int
  =
    Array.initialize
      x
      (\x_
        ->
          Array.initialize
            y
            (\y_
              ->
                fromMatrixToPatrix_helper_2 x_y_int x_ y_))

-- x と y に対応する Pindex の値を返す。
-- x が範囲を外れた時は、 Null を返す。
-- y が範囲を外れた時は、それが y < 0 であるとき、つまり上側だった時は、 x が 0, 1, 2, 3, ... であるとき、 Null, Pindex 0, Pindex 1, Pindex 2, ... という結果になる。それが Array.length y_int <= y であるとき、つまり下側だったときは、 Null を返す。
fromMatrixToPatrix_helper_2
  : Array (Array Int) -> Int -> Int -> Case Pindex
fromMatrixToPatrix_helper_2 x_y_int x y
  =
    case Array.get x x_y_int of
      Nothing
        ->
          if 0 <= x && x < Array.length x_y_int
            then ImpossibleCase
            else PossibleCase Null
      Just y_int
        ->
          case Array.get y y_int of
            Nothing ->
              if 0 <= y
                then
                  if y < Array.length y_int
                    then ImpossibleCase
                    else PossibleCase Null
                else
                  if 0 <= x && x < Array.length x_y_int
                    then
                      if x == 0
                        then PossibleCase Null
                        else PossibleCase (Pindex (x - 1))
                    else
                      ImpossibleCase
            Just int
              -> fromMatrixToPatrix_helper_3 x_y_int x y y_int int (x - 1)

-- x と y の親を探索する。
-- p が範囲を外れた時は、 p < x かつ x は範囲を外れていないという事実より、 p < 0 であり、ここまで探索の手が伸びるということは、 x の y での親はないということである。
-- p がずれたことにより y が範囲を外れた時は、それが y < 0 である、つまり上側だった時は、 fromMatrixToPatrix_helper_2 が示しているような 0 ← 1 ← 2 ← 3 ← ... の構造に従って親を判定する。それが、 Array.length y_int <= y である、つまり下側だった時は、そこは底値で埋め尽くされているという考え方に従って親を判定する。これらは、結果的に同じ処理となる。
-- p は関数の状態を保持する役割を持つ引数である。 fromMatrixToPatrix_helper_2 と再帰の構造より p < x である。
fromMatrixToPatrix_helper_3
  : Array (Array Int) -> Int -> Int -> Array Int -> Int -> Int -> Case Pindex
fromMatrixToPatrix_helper_3 x_y_int x y y_int int p
  =
    case Array.get p x_y_int of
      Nothing -> PossibleCase Null
      Just y_int_
        ->
          case Array.get y y_int_ of
            Nothing
              ->
                if 0 <= y && y < Array.length y_int
                  then ImpossibleCase
                  else
                    case fromMatrixToPatrix_helper_4 x_y_int x (y - 1) of
                      ImpossibleCase -> ImpossibleCase
                      PossibleCase is_ancestor
                        ->
                          if is_ancestor p
                            then PossibleCase (Pindex p)
                            else
                              fromMatrixToPatrix_helper_3
                                x_y_int
                                x
                                y
                                y_int
                                int
                                (p - 1)
            Just int_
              ->
                case fromMatrixToPatrix_helper_4 x_y_int x (y - 1) of
                  ImpossibleCase -> ImpossibleCase
                  PossibleCase is_ancestor
                    ->
                      if int_ < int && is_ancestor p
                        then PossibleCase (Pindex p)
                        else
                          fromMatrixToPatrix_helper_3
                            x_y_int
                            x
                            y
                            y_int
                            int
                            (p - 1)

-- x と y の先祖の集合を計算する。集合は、その特性関数で表される。
fromMatrixToPatrix_helper_4
  : Array (Array Int) -> Int -> Int -> Case (Int -> Bool)
fromMatrixToPatrix_helper_4 x_y_int x y
  =
    case fromMatrixToPatrix_helper_2 x_y_int x y of
      ImpossibleCase -> ImpossibleCase
      PossibleCase xp -> case xp of
        Null -> PossibleCase (\x__ -> x == x__)
        Pindex x_
          ->
            case fromMatrixToPatrix_helper_4 x_y_int x_ y of
              ImpossibleCase -> ImpossibleCase
              PossibleCase is_ancestor
                -> PossibleCase (\x__ -> x == x__ || is_ancestor x__)

{-| 或るパトリックスを或る行列に変換します。 -}
fromPatrixToMatrix : Patrix -> Matrix
fromPatrixToMatrix = Debug.todo "to do implement"

{-| 或るリストを生のまま行列へ変換します。

この関数は `Matrix` の規約を破る値を返す可能性があります。
-}
fromListToMatrixRawly : Int -> Int -> List (List Int) -> Matrix
fromListToMatrixRawly x y x_y_int = Matrix x y (fromListToArray x_y_int)

{-| 或るリストを生のままパトリックスへ変換します。

この関数は `Patrix` の規約を破る値を返す可能性があります。
-}
fromListToPatrixRawly : Int -> Int -> List (List Pindex) -> Patrix
fromListToPatrixRawly x y x_y_pindex = Patrix x y (fromListToArray x_y_pindex)
