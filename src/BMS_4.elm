module BMS_4
  exposing
    (
      fromArrayToList,
      fromListToArray,
      Nat,
      RawMatrix,
      Matrix,
      fromMatrixToArray,
      fromArrayToMatrix,
      fromMatrixToList,
      fromListToMatrix,
      expand,
      Pindex (..),
      RawPatrix,
      Patrix,
      calcPatrixFromMatrix,
      calcParentOnPatrixFromRawMatrix,
      calcAncestorSetOnPatrixFromRawMatrix,
      calcMatrixFromPatrix,
      calcElementOnMatrixFromRawPatrix,
      fromListToMatrixRawly,
      fromListToPatrixRawly
    )

import Case exposing (Case (..))

import Array exposing (Array)
import Array.Extra.Folding as Array

import BMS_4.Parsing as Parsing

{-| 或る配列を或るリストへ変換します。 -}
fromArrayToList : Array (Array a) -> List (List a)
fromArrayToList array = Array.toList (Array.map Array.toList array)

{-| 或るリストを或る配列へ変換します。 -}
fromListToArray : List (List a) -> Array (Array a)
fromListToArray list = Array.map Array.fromList (Array.fromList list)

{-| これは自然数です。 -}
type Nat = Nat Int

{-| これはバシク行列システムの行列を表す生の型です。

この型の値を直接扱うことは `Matrix` 型の規約を破るような値を生み出すことに繋がるため、このモジュールが提供する関数だけを使って扱ってください。
-}
type alias RawMatrix = Array (Array Int)

{-| これはバシク行列システムにおける行列です。

`Matrix` 型の値は `verifyMatrix` を満たしていなければなりません。すなわち、 `Matrix x y x_y_int` は、その行の長さが `x` であり、その列の長さが `y` でなければなりません。
-}
type Matrix = Matrix Int Int RawMatrix

{-| 或る値が `Matrix` 型の規約を満たしているか検証します。 -}
verifyMatrix : Matrix -> Bool
verifyMatrix matrix
  =
    case matrix of
      Matrix x y x_y_int
        ->
          True
            && Array.length x_y_int == x
            && Array.all (\a -> Array.length a == y) x_y_int

{-| 或る行列を或る配列へ変換します。 -}
fromMatrixToArray : Matrix -> Array (Array Int)
fromMatrixToArray matrix
  =
    case matrix of
      Matrix x y x_y_int -> x_y_int

{-| 或る配列を或る行列へ変換します。

其の配列が行列を正しく表していれば、そのようになります。もし、列の長さが不揃いであれば、最も長い列が行数の基準となり、それに合わせて他の列が底値でもってパディングされます。もし、行の長さがゼロであれば、行数（列の長さ）は不定値になりますので、デフォルトの値としてゼロとなります。

底値は、其の配列に含まれる最小の値です。ゼロを使わないのは、其の配列が表現する行列のトポロジーを可能な限り保つためです。
-}
fromArrayToMatrix : Array (Array Int) -> Matrix
fromArrayToMatrix x_y_int
  =
    let
      x = Array.length x_y_int
      y = Maybe.withDefault 0 (Array.maximum (Array.map Array.length x_y_int))
      e
        =
          Maybe.withDefault 0
            (Array.minimum
              (Array.map
                (\y_int
                  ->
                    Maybe.withDefault
                      0
                      (Array.minimum y_int))
                x_y_int))
    in
      Matrix x y (Array.map (fromArrayToMatrix_helper_1 y e) x_y_int)

fromArrayToMatrix_helper_1 : Int -> Int -> Array Int -> Array Int
fromArrayToMatrix_helper_1 y e y_int
  = Array.initialize y (\i -> Maybe.withDefault e (Array.get i y_int))

{-| 或る行列を或るリストへと変換します。 -}
fromMatrixToList : Matrix -> List (List Int)
fromMatrixToList matrix
  =
    case matrix of
      Matrix x y x_y_int -> fromArrayToList x_y_int

{-| 或るリストを或る行列へと変換します。

其のリストが行列を正しく表していれば、そのようになります。もし、列の長さが不揃いであれば、最も長い列が行数の基準となり、それに合わせて他の列が底値でもってパディングされます。もし、行の長さがゼロであれば、行数（列の長さ）は不定値になりますので、デフォルトの値としてゼロとなります。

底値は、其のリストに含まれる最小の値です。ゼロを使わないのは、其のリストが表現する行列のトポロジーを可能な限り保つためです。
-}
fromListToMatrix : List (List Int) -> Matrix
fromListToMatrix x_y_int
  =
    let
      x = List.length x_y_int
      y = Maybe.withDefault 0 (List.maximum (List.map List.length x_y_int))
      e
        =
          Maybe.withDefault 0
            (List.minimum
              (List.map
                (\y_int
                  ->
                    Maybe.withDefault
                      0
                      (List.minimum y_int))
                x_y_int))
    in
      Matrix
        x
        y
        (Array.fromList (List.map (fromListToMatrix_helper_1 y e) x_y_int))

fromListToMatrix_helper_1 : Int -> Int -> List Int -> Array Int
fromListToMatrix_helper_1 y e y_int
  = Array.initialize y (fromListToMatrix_helper_2 e y_int)

-- fromListToMatrix_helper_1 により 0 <= i である。
-- list-extra 8.3.0 の getAt で実装することも出来るが、上記の条件を使って実装を単純にしている。
fromListToMatrix_helper_2 : Int -> List Int -> Int -> Int
fromListToMatrix_helper_2 e y_int i
  =
    case y_int of
      [] -> e
      int :: y_int_
        ->
          if i == 0
            then int
            else fromListToMatrix_helper_2 e y_int_ (i - 1)

{-| 或る行列を或る自然数により展開します。 `Just` で包んだ結果を返します。其の自然数が其の行列の共終タイプ以上なら `Nothing` を返します。 -}
expand : Matrix -> Nat -> Maybe Matrix
expand n x = expand n x

{-| これはピンデックスです。ピンデックスは或る行列の要素へのポインターを意味します。 -}
type Pindex = Null | Pindex Int

{-| これはパトリックスを表す生の型です。

この型の値を直接扱うことは `Patrix` 型の規約を破るような値を生み出すことに繋がるため、このモジュールが提供する関数だけを使って扱ってください。
-}
type alias RawPatrix = Array (Array Pindex)

{-| これはパトリックスです。パトリックスはピンデックスの行列を意味します。

`Patrix x y x_y_pindex` は、その行の長さが `x` であり、その列の長さが `y` であり、それぞれの行が正しい木構造を構成していて、上部の行の木構造に下部の行の木構造が埋め込み可能でなければなりません。
-}
type Patrix = Patrix Int Int RawPatrix

{-| 或る行列をパトリックスへ変換します。 -}
calcPatrixFromMatrix : Matrix -> Case Patrix
calcPatrixFromMatrix matrix
  =
    case matrix of
      Matrix x y x_y_int
        ->
          case calcPatrixFromMatrix_helper_1 x y x_y_int of
            ImpossibleCase -> ImpossibleCase
            PossibleCase x_y_pindex -> PossibleCase (Patrix x y x_y_pindex)

calcPatrixFromMatrix_helper_1
  : Int -> Int -> RawMatrix -> Case RawPatrix
calcPatrixFromMatrix_helper_1 x y x_y_int
  =
    Case.traverseArray (\case_x -> case_x)
      (Array.map (Case.traverseArray (\case_x -> case_x))
        (calcPatrixFromMatrix_helper_2 x y x_y_int))

calcPatrixFromMatrix_helper_2
  : Int -> Int -> RawMatrix -> Array (Array (Case Pindex))
calcPatrixFromMatrix_helper_2 x y x_y_int
  =
    Array.initialize
      x
      (\x_
        ->
          Array.initialize
            y
            (\y_
              ->
                calcParentOnPatrixFromRawMatrix x_y_int x_ y_))

{-| 或る `RawMatrix` と、それの一つの要素を特定する二つの整数 `x` と `y` から、その要素の親を表す或る `Pindex` を計算し、それを返します。

`x` が範囲を外れている時は、 `Null` を返します。 `x` が範囲を外れていなくて `y` が `0` 未満である時は、 `x` が `0` であるならば `Null` となり、そうでないならば `Pindex (x - 1)` となります。 `x` が範囲の中にあって `y` が `x` が指す列の長さ以上である時は、 `Null` になります。
-}
calcParentOnPatrixFromRawMatrix
  : RawMatrix -> Int -> Int -> Case Pindex
calcParentOnPatrixFromRawMatrix x_y_int x y
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
              ->
                calcParentOnPatrixFromRawMatrix_helper_1
                  x_y_int
                  x
                  y
                  y_int
                  int
                  (x - 1)

-- x と y の親を探索する。
-- p が範囲を外れた時は、 p < x かつ x は範囲を外れていないという事実より、 p < 0 であり、ここまで探索の手が伸びるということは、 x の y での親はないということである。
-- p がずれたことにより y が範囲を外れた時は、それが y < 0 である、つまり上側だった時は、 calcParentOnPatrixFromRawMatrix が示しているような 0 ← 1 ← 2 ← 3 ← ... の構造に従って親を判定する。それが、 Array.length y_int <= y である、つまり下側だった時は、そこは底値で埋め尽くされているという考え方に従って親を判定する。これらは、結果的に同じ処理となる。
-- p は関数の状態を保持する役割を持つ引数である。 calcParentOnPatrixFromRawMatrix と再帰の構造より p < x である。
calcParentOnPatrixFromRawMatrix_helper_1
  : RawMatrix -> Int -> Int -> Array Int -> Int -> Int -> Case Pindex
calcParentOnPatrixFromRawMatrix_helper_1 x_y_int x y y_int int p
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
                    case
                      calcAncestorSetOnPatrixFromRawMatrix x_y_int x (y - 1)
                    of
                      ImpossibleCase -> ImpossibleCase
                      PossibleCase is_ancestor
                        ->
                          if is_ancestor p
                            then PossibleCase (Pindex p)
                            else
                              calcParentOnPatrixFromRawMatrix_helper_1
                                x_y_int
                                x
                                y
                                y_int
                                int
                                (p - 1)
            Just int_
              ->
                case calcAncestorSetOnPatrixFromRawMatrix x_y_int x (y - 1) of
                  ImpossibleCase -> ImpossibleCase
                  PossibleCase is_ancestor
                    ->
                      if int_ < int && is_ancestor p
                        then PossibleCase (Pindex p)
                        else
                          calcParentOnPatrixFromRawMatrix_helper_1
                            x_y_int
                            x
                            y
                            y_int
                            int
                            (p - 1)

{-| 或る `RawMatrix` と、それの一つの要素を特定する二つの整数 `x` と `y` から、その要素の先祖を表す或る集合 (`Int -> Bool`) を計算し、それを返します。 -}
calcAncestorSetOnPatrixFromRawMatrix
  : RawMatrix -> Int -> Int -> Case (Int -> Bool)
calcAncestorSetOnPatrixFromRawMatrix x_y_int x y
  =
    case calcParentOnPatrixFromRawMatrix x_y_int x y of
      ImpossibleCase -> ImpossibleCase
      PossibleCase xp -> case xp of
        Null -> PossibleCase (\x__ -> x == x__)
        Pindex x_
          ->
            if x_ < x
              then
                case calcAncestorSetOnPatrixFromRawMatrix x_y_int x_ y of
                  ImpossibleCase -> ImpossibleCase
                  PossibleCase is_ancestor
                    -> PossibleCase (\x__ -> x == x__ || is_ancestor x__)
              else
                PossibleCase (\x__ -> x == x__)

{-| 或るパトリックスを或る行列に変換します。 -}
calcMatrixFromPatrix : Patrix -> Case Matrix
calcMatrixFromPatrix patrix
  =
    case patrix of
      Patrix x y x_y_pindex
        ->
          case calcMatrixFromPatrix_helper_1 x y x_y_pindex of
            ImpossibleCase -> ImpossibleCase
            PossibleCase x_y_int -> PossibleCase (Matrix x y x_y_int)

calcMatrixFromPatrix_helper_1
  : Int -> Int -> RawPatrix -> Case RawMatrix
calcMatrixFromPatrix_helper_1 x y x_y_pindex
  =
    Case.traverseArray (\case_x -> case_x)
      (Array.map (Case.traverseArray (\case_x -> case_x))
        (calcMatrixFromPatrix_helper_2 x y x_y_pindex))

calcMatrixFromPatrix_helper_2
  : Int -> Int -> RawPatrix -> Array (Array (Case Int))
calcMatrixFromPatrix_helper_2 x y x_y_pindex
  =
    Array.initialize
      x
      (\x_
        ->
          Array.initialize
            y
            (\y_
              ->
                calcElementOnMatrixFromRawPatrix x_y_pindex x_ y_))

{-| 或る `RawPatrix` と、それの一つの要素を特定する二つの整数 `x` と `y` から、其の `RawPatrix` に対応する行列の、その要素に対応する要素を表す、或る `Int` を計算し、それを返します。

`x` が範囲を外れている時は、 `0` を返します。 `x` が範囲を外れていなくて `y` が範囲を外れている時は、 `0` を返します。
-}
calcElementOnMatrixFromRawPatrix
  : RawPatrix -> Int -> Int -> Case Int
calcElementOnMatrixFromRawPatrix x_y_pindex x y
  =
    case Array.get x x_y_pindex of
      Nothing
        ->
          if 0 <= x && x < Array.length x_y_pindex
            then ImpossibleCase
            else PossibleCase 0
      Just y_pindex
        ->
          case Array.get y y_pindex of
            Nothing
              ->
                if 0 <= y && y < Array.length y_pindex
                  then ImpossibleCase
                  else PossibleCase 0
            Just pindex
              ->
                case pindex of
                  Null -> PossibleCase 0
                  Pindex p
                    ->
                      if p < x
                        then
                          case
                            calcElementOnMatrixFromRawPatrix x_y_pindex p y
                          of
                            ImpossibleCase -> ImpossibleCase
                            PossibleCase int -> PossibleCase (int + 1)
                        else PossibleCase 0

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
