module BMS_4
  exposing
    (
      RawMatrix
    ,
      toRawMatrixFromList
    ,
      toListFromRawMatrix
    ,
      Matrix (..)
    ,
      compareMatrix
    ,
      verifyMatrix
    ,
      toMatrixFromRawMatrix
    ,
      toRawMatrixFromMatrix
    ,
      calcCoftypeOfMatrix
    ,
      expandMatrix
    ,
      Pindex (..)
    ,
      RawPatrix
    ,
      toRawPatrixFromList
    ,
      toListFromRawPatrix
    ,
      Patrix (..)
    ,
      calcPatrixFromMatrix
    ,
      MemoCalcPatrixFromMatrix
    ,
      emptyMemoCalcPatrixFromMatrix
    ,
      getMemoCalcParentOnPatrixFromRawMatrix
    ,
      getMemoCalcAncestorSetOnPatrixFromRawMatrix
    ,
      insertMemoCalcParentOnPatrixFromRawMatrix
    ,
      insertMemoCalcAncestorSetOnPatrixFromRawMatrix
    ,
      calcParentOnPatrixFromRawMatrix
    ,
      calcAncestorSetOnPatrixFromRawMatrix
    ,
      calcParentOnPatrixFromRawMatrixWithMemo
    ,
      calcAncestorSetOnPatrixFromRawMatrixWithMemo
    ,
      calcMatrixFromPatrix
    ,
      calcElementOnMatrixFromRawPatrix
    ,
      calcCoftypeOfPatrix
    ,
      calcBadRootOfPatrix
    ,
      expandPatrix
    ,
      notation
    )

import Result exposing (Result)

import Case exposing (Case (..))

import Dict exposing (Dict)

import Array exposing (Array)
import Array.Extra as Array
import Array.Extra.Folding as Array

import Notation
  exposing
    (
      Nat (..)
    ,
      toNatFromInt
    ,
      toIntFromNat
    ,
      Coftype (..)
    ,
      compareNat
    ,
      OutOfIndexError (..)
    ,
      Notation
    ,
      Maxipointed (..)
    ,
      compareMaxipointed
    ,
      expandMaxipointed
    )

{-| これはバシク行列システムの行列を表す生の型です。

この型の値を直接扱うことは `Matrix` 型の規約を破るような値を生み出すことに繋がるため、このモジュールが提供する関数だけを使って扱ってください。
-}
type alias RawMatrix = Array (Array Int)

{-| 或るリストを或る生の行列へ変換します。
-}
toRawMatrixFromList : List (List Int) -> RawMatrix
toRawMatrixFromList list = Array.map Array.fromList (Array.fromList list)

{-| 或る生の行列を或るリストへ変換します。
-}
toListFromRawMatrix : RawMatrix -> List (List Int)
toListFromRawMatrix array = Array.toList (Array.map Array.toList array)

{-| これはバシク行列システムにおける行列です。

`Matrix` 型の値は `verifyMatrix` を満たしていなければなりません。すなわち、 `Matrix x y x_y_int` は、その行の長さが `x` であり、その列の長さが `y` でなければなりません。

構築子は `Matrix` 型の規約が守られていることが保証されていないため、テスト以外で使ってはいけません。
-}
type Matrix = Matrix Int Int RawMatrix

{-| 行列同士を比較します。 -}
compareMatrix : Matrix -> Matrix -> Order
compareMatrix (Matrix _ _ x) (Matrix _ _ y) = compare (toListFromRawMatrix x) (toListFromRawMatrix y)

{-| 或る値が `Matrix` 型の規約を満たしているか検証します。
-}
verifyMatrix : Matrix -> Bool
verifyMatrix (Matrix x y x_y_int) = Array.length x_y_int == x && Array.all (\a -> Array.length a == y) x_y_int

{-| 或る生の行列を或る行列へ変換します。

其の生の行列が行列を正しく表していれば、そのようになります。もし、列の長さが不揃いであれば、最も長い列が行数の基準となり、それに合わせて他の列が底値でもってパディングされます。もし、行の長さがゼロであれば、行数（列の長さ）は不定値になりますので、デフォルトの値としてゼロとなります。

底値は、其の生の行列に含まれる最小の値です。ゼロを使わないのは、其の生の行列が表現する行列のトポロジーを可能な限り保つためです。
-}
toMatrixFromRawMatrix : RawMatrix -> Matrix
toMatrixFromRawMatrix x_y_int
  =
    let
      x = Array.length x_y_int
      y = Maybe.withDefault 0 (Array.maximum (Array.map Array.length x_y_int))
      e = Maybe.withDefault 0 (Array.minimum (Array.map (\y_int -> Maybe.withDefault 0 (Array.minimum y_int)) x_y_int))
    in
      Matrix x y (Array.map (fromArrayToMatrix_helper_1 y e) x_y_int)

fromArrayToMatrix_helper_1 : Int -> Int -> Array Int -> Array Int
fromArrayToMatrix_helper_1 y e y_int = Array.initialize y (\i -> Maybe.withDefault e (Array.get i y_int))

{-| 或る行列を或る生の行列へ変換します。
-}
toRawMatrixFromMatrix : Matrix -> RawMatrix
toRawMatrixFromMatrix (Matrix x y x_y_int) = x_y_int

{-| 或る行列の共終タイプを計算します。 -}
calcCoftypeOfMatrix : Matrix -> Case Coftype
calcCoftypeOfMatrix matrix
  =
    case calcPatrixFromMatrix matrix of
      PossibleCase patrix -> PossibleCase (calcCoftypeOfPatrix patrix)
      ImpossibleCase -> ImpossibleCase

{-| 或る行列を或る自然数により展開します。 `Just` で包んだ結果を返します。其の自然数が其の行列の共終タイプ以上なら `Nothing` を返します。
-}
expandMatrix : Matrix -> Nat -> Case (Result (OutOfIndexError Matrix) Matrix)
expandMatrix matrix n
  =
    case calcPatrixFromMatrix matrix of
      PossibleCase patrix
        ->
          case expandPatrix patrix n of
            PossibleCase result_patrix_
              ->
                case result_patrix_ of
                  Ok patrix_
                    ->
                      case calcMatrixFromPatrix patrix_ of
                        PossibleCase matrix_ -> PossibleCase (Ok matrix_)
                        ImpossibleCase -> ImpossibleCase
                  Err (OutOfIndexError patrix_ n_ coftype)
                    ->
                      case calcMatrixFromPatrix patrix_ of
                        PossibleCase matrix_ -> PossibleCase (Err (OutOfIndexError matrix_ n_ coftype))
                        ImpossibleCase -> ImpossibleCase
            ImpossibleCase -> ImpossibleCase
      ImpossibleCase -> ImpossibleCase

{-| これはピンデックスです。ピンデックスは或る行列の要素へのポインターを意味します。
-}
type Pindex = Null | Pindex Int

{-| これはパトリックスを表す生の型です。

この型の値を直接扱うことは `Patrix` 型の規約を破るような値を生み出すことに繋がるため、このモジュールが提供する関数だけを使って扱ってください。
-}
type alias RawPatrix = Array (Array Pindex)

{-| 或るリストを或る生のパトリックスへ変換します。
-}
toRawPatrixFromList : List (List Pindex) -> RawPatrix
toRawPatrixFromList list = Array.map Array.fromList (Array.fromList list)

{-| 或る生のパトリックスを或るリストへ変換します。
-}
toListFromRawPatrix : RawPatrix -> List (List Pindex)
toListFromRawPatrix array = Array.toList (Array.map Array.toList array)

{-| これはパトリックスです。パトリックスはピンデックスの行列を意味します。

`Patrix x y x_y_pindex` は、その行の長さが `x` であり、その列の長さが `y` であり、それぞれの行が正しい木構造を構成していて、上部の行の木構造に下部の行の木構造が埋め込み可能でなければなりません。

構築子は `Patrix` 型の規約が守られていることが保証されていないため、テスト以外で使ってはいけません。
-}
type Patrix = Patrix Int Int RawPatrix

{-| 或る行列から或るパトリックスを計算します。
-}
calcPatrixFromMatrix : Matrix -> Case Patrix
calcPatrixFromMatrix matrix
  =
    case matrix of
      Matrix x y x_y_int
        ->
          case calcPatrixFromMatrix_helper_1 x y x_y_int of
            ImpossibleCase -> ImpossibleCase
            PossibleCase x_y_pindex -> PossibleCase (Patrix x y x_y_pindex)

{-| `calcPatrixFromMatrix` の内部計算のメモです。
-}
type alias MemoCalcPatrixFromMatrix
  =
    {
      parent : Dict (Int, Int) Pindex
    ,
      ancestorSet : Dict (Int, Int) (Int -> Bool)
    }

{-| 其の空の `MemoCalcPatrixFromMatrix` です。
-}
emptyMemoCalcPatrixFromMatrix : MemoCalcPatrixFromMatrix
emptyMemoCalcPatrixFromMatrix = { parent = Dict.empty, ancestorSet = Dict.empty }

{-| 或る `MemoCalcPatrixFromMatrix` から `calcParentOnPatrixFromRawMatrix` の結果を取り出します。
-}
getMemoCalcParentOnPatrixFromRawMatrix : MemoCalcPatrixFromMatrix -> (Int, Int) -> Maybe Pindex
getMemoCalcParentOnPatrixFromRawMatrix memo k = Dict.get k memo.parent

{-| 或る `MemoCalcPatrixFromMatrix` から `calcAncestorSetOnPatrixFromRawMatrix` の結果を取り出します。
-}
getMemoCalcAncestorSetOnPatrixFromRawMatrix : MemoCalcPatrixFromMatrix -> (Int, Int) -> Maybe (Int -> Bool)
getMemoCalcAncestorSetOnPatrixFromRawMatrix memo k = Dict.get k memo.ancestorSet

{-| 或る `MemoCalcPatrixFromMatrix` に `calcParentOnPatrixFromRawMatrix` の結果をメモします。
-}
insertMemoCalcParentOnPatrixFromRawMatrix : MemoCalcPatrixFromMatrix -> (Int, Int) -> Pindex -> MemoCalcPatrixFromMatrix
insertMemoCalcParentOnPatrixFromRawMatrix memo k v = { memo | parent = Dict.insert k v memo.parent }

{-| 或る `MemoCalcPatrixFromMatrix` に `calcAncestorSetOnPatrixFromRawMatrix` の結果をメモします。
-}
insertMemoCalcAncestorSetOnPatrixFromRawMatrix : MemoCalcPatrixFromMatrix -> (Int, Int) -> (Int -> Bool) -> MemoCalcPatrixFromMatrix
insertMemoCalcAncestorSetOnPatrixFromRawMatrix memo k v = { memo | ancestorSet = Dict.insert k v memo.ancestorSet }

calcPatrixFromMatrix_helper_1 : Int -> Int -> RawMatrix -> Case RawPatrix
calcPatrixFromMatrix_helper_1 x y x_y_int
  =
    case calcPatrixFromMatrix_helper_2 x y x_y_int emptyMemoCalcPatrixFromMatrix of
      PossibleCase (x_y_patrix, memo) -> PossibleCase x_y_patrix
      ImpossibleCase -> ImpossibleCase

calcPatrixFromMatrix_helper_2 : Int -> Int -> RawMatrix -> MemoCalcPatrixFromMatrix -> Case (Array (Array Pindex), MemoCalcPatrixFromMatrix)
calcPatrixFromMatrix_helper_2 x y x_y_int
  =
    let
      pindex x_ y_ = calcParentOnPatrixFromRawMatrixWithMemo x_y_int x_ y_
      y_pindex x_ = Case.initializeArrayWithCaseWithState y (\y_ -> pindex x_ y_)
      x_y_pindex = Case.initializeArrayWithCaseWithState x (\x_ -> y_pindex x_)
    in
      x_y_pindex

{-| 或る `RawMatrix` と、それの一つの要素を特定する二つの整数 `x` と `y` から、その要素の親を表す或る `Pindex` を計算し、それを返します。

`x` が範囲を外れている時は、 `Null` を返します。 `x` が範囲を外れていなくて `y` が `0` 未満である時は、 `x` が `0` であるならば `Null` となり、そうでないならば `Pindex (x - 1)` となります。 `x` が範囲の中にあって `y` が `x` が指す列の長さ以上である時は、 `Null` になります。
-}
calcParentOnPatrixFromRawMatrix : RawMatrix -> Int -> Int -> Case Pindex
calcParentOnPatrixFromRawMatrix x_y_int x y
  =
    case calcParentOnPatrixFromRawMatrixWithMemo x_y_int x y emptyMemoCalcPatrixFromMatrix of
      PossibleCase (pindex, memo) -> PossibleCase pindex
      ImpossibleCase -> ImpossibleCase

{-| 或る `RawMatrix` と、それの一つの要素を特定する二つの整数 `x` と `y` から、その要素の先祖を表す或る集合 (`Int -> Bool`) を計算し、それを返します。
-}
calcAncestorSetOnPatrixFromRawMatrix : RawMatrix -> Int -> Int -> Case (Int -> Bool)
calcAncestorSetOnPatrixFromRawMatrix x_y_int x y
  =
    case calcAncestorSetOnPatrixFromRawMatrixWithMemo x_y_int x y emptyMemoCalcPatrixFromMatrix of
      PossibleCase (is_ancestor, memo) -> PossibleCase is_ancestor
      ImpossibleCase -> ImpossibleCase

{-| 或る `RawMatrix` と、それの一つの要素を特定する二つの整数 `x` と `y` から、その要素の親を表す或る `Pindex` を計算し、それを返します。メモ化しています。

`x` が範囲を外れている時は、 `Null` を返します。 `x` が範囲を外れていなくて `y` が `0` 未満である時は、 `x` が `0` であるならば `Null` となり、そうでないならば `Pindex (x - 1)` となります。 `x` が範囲の中にあって `y` が `x` が指す列の長さ以上である時は、 `Null` になります。
-}
calcParentOnPatrixFromRawMatrixWithMemo : RawMatrix -> Int -> Int -> MemoCalcPatrixFromMatrix -> Case (Pindex, MemoCalcPatrixFromMatrix)
calcParentOnPatrixFromRawMatrixWithMemo x_y_int x y memo
  =
    let
      get = getMemoCalcParentOnPatrixFromRawMatrix memo (x, y)
      set = insertMemoCalcParentOnPatrixFromRawMatrix memo (x, y)
      parent_0 = calcParentOnPatrixFromRawMatrixWithMemo_helper_1
    in
      case get of
        Just pindex -> PossibleCase (pindex, memo)
        Nothing
          ->
            case Array.get x x_y_int of
              Just y_int
                ->
                  case Array.get y y_int of
                    Just int -> parent_0 x_y_int x y y_int int (x - 1) memo
                    Nothing ->
                      if 0 <= y
                        then
                          if y < Array.length y_int
                            then ImpossibleCase
                            else PossibleCase (Null, set Null)
                        else
                          if 0 <= x && x < Array.length x_y_int
                            then
                              if x == 0
                                then PossibleCase (Null, set Null)
                                else PossibleCase (Pindex (x - 1), set (Pindex (x - 1)))
                            else
                              ImpossibleCase
              Nothing
                ->
                  if 0 <= x && x < Array.length x_y_int
                    then ImpossibleCase
                    else PossibleCase (Null, set Null)

-- x と y の親を探索する。
-- p が範囲を外れた時は、 p < x かつ x は範囲を外れていないという事実より、 p < 0 であり、ここまで探索の手が伸びるということは、 x の y での親はないということである。
-- p がずれたことにより y が範囲を外れた時は、それが y < 0 である、つまり上側だった時は、 calcParentOnPatrixFromRawMatrixWithMemo が示しているような 0 ← 1 ← 2 ← 3 ← ... の構造に従って親を判定する。それが、 Array.length y_int <= y である、つまり下側だった時は、そこは底値で埋め尽くされているという考え方に従って親を判定する。これらは、結果的に同じ処理となる。
-- p は関数の状態を保持する役割を持つ引数である。 calcParentOnPatrixFromRawMatrixWithMemo と再帰の構造より p < x である。
calcParentOnPatrixFromRawMatrixWithMemo_helper_1 : RawMatrix -> Int -> Int -> Array Int -> Int -> Int -> MemoCalcPatrixFromMatrix -> Case (Pindex, MemoCalcPatrixFromMatrix)
calcParentOnPatrixFromRawMatrixWithMemo_helper_1 x_y_int x y y_int int p memo
  =
    let
      set = insertMemoCalcParentOnPatrixFromRawMatrix memo (x, y)
      parent = calcAncestorSetOnPatrixFromRawMatrixWithMemo
      parent_0 = calcParentOnPatrixFromRawMatrixWithMemo_helper_1
    in
      case Array.get p x_y_int of
        Just y_int_
          ->
            case Array.get y y_int_ of
              Just int_
                ->
                  case parent x_y_int x (y - 1) memo of
                    PossibleCase (is_ancestor, memo_)
                      ->
                        if int_ < int && is_ancestor p
                          then PossibleCase (Pindex p, set (Pindex p))
                          else parent_0 x_y_int x y y_int int (p - 1) memo_
                    ImpossibleCase -> ImpossibleCase
              Nothing
                ->
                  if 0 <= y && y < Array.length y_int_
                    then ImpossibleCase
                    else
                      case parent x_y_int x (y - 1) memo of
                        PossibleCase (is_ancestor, memo_)
                          ->
                            if is_ancestor p
                              then PossibleCase (Pindex p, set (Pindex p))
                              else parent_0 x_y_int x y y_int int (p - 1) memo_
                        ImpossibleCase -> ImpossibleCase
        Nothing -> PossibleCase (Null, set Null)

{-| 或る `RawMatrix` と、それの一つの要素を特定する二つの整数 `x` と `y` から、その要素の先祖を表す或る集合 (`Int -> Bool`) を計算し、それを返します。メモ化しています。
-}
calcAncestorSetOnPatrixFromRawMatrixWithMemo : RawMatrix -> Int -> Int -> MemoCalcPatrixFromMatrix -> Case (Int -> Bool, MemoCalcPatrixFromMatrix)
calcAncestorSetOnPatrixFromRawMatrixWithMemo x_y_int x y memo
  =
    let
      get = getMemoCalcAncestorSetOnPatrixFromRawMatrix memo (x, y)
      set = insertMemoCalcAncestorSetOnPatrixFromRawMatrix memo (x, y)
      parent = calcParentOnPatrixFromRawMatrixWithMemo
      ancestorSet = calcAncestorSetOnPatrixFromRawMatrixWithMemo
    in
      case get of
        Just is_ancestor -> PossibleCase (is_ancestor, memo)
        Nothing
          ->
            case parent x_y_int x y memo of
              PossibleCase (xp, memo_) -> case xp of
                Null
                  ->
                    let
                      v = \x__ -> x == x__
                    in
                      PossibleCase (v, set v)
                Pindex x_
                  ->
                    if x_ < x
                      then
                        case ancestorSet x_y_int x_ y memo_ of
                          PossibleCase (v, memo__)
                            ->
                              let
                                v_ = \x__ -> x == x__ || v x__
                              in
                                PossibleCase (v_, set v_)
                          ImpossibleCase -> ImpossibleCase
                      else
                        let
                          v = \x__ -> x == x__
                        in
                          PossibleCase (v, set v)
              ImpossibleCase -> ImpossibleCase

{-| 或るパトリックスから或る行列を計算します。
-}
calcMatrixFromPatrix : Patrix -> Case Matrix
calcMatrixFromPatrix (Patrix x y x_y_pindex)
  =
    case calcMatrixFromPatrix_helper_1 x y x_y_pindex of
      PossibleCase x_y_int -> PossibleCase (Matrix x y x_y_int)
      ImpossibleCase -> ImpossibleCase

calcMatrixFromPatrix_helper_1 : Int -> Int -> RawPatrix -> Case RawMatrix
calcMatrixFromPatrix_helper_1 x y x_y_pindex
  =
    let
      int x_ y_ = calcElementOnMatrixFromRawPatrix x_y_pindex x_ y_
      y_int x_ = Case.initializeArrayWithCase y (\y_ -> int x_ y_)
      x_y_int = Case.initializeArrayWithCase x (\x_ -> y_int x_)
    in
      x_y_int

{-| 或る `RawPatrix` と、それの一つの要素を特定する二つの整数 `x` と `y` から、其の `RawPatrix` に対応する行列の、その要素に対応する要素を表す、或る `Int` を計算し、それを返します。

`x` が範囲を外れている時は、 `0` を返します。 `x` が範囲を外れていなくて `y` が範囲を外れている時は、 `0` を返します。
-}
calcElementOnMatrixFromRawPatrix : RawPatrix -> Int -> Int -> Case Int
calcElementOnMatrixFromRawPatrix x_y_pindex x y
  =
    case Array.get x x_y_pindex of
      Just y_pindex
        ->
          case Array.get y y_pindex of
            Just pindex
              ->
                case pindex of
                  Null -> PossibleCase 0
                  Pindex p
                    ->
                      if p < x
                        then
                          case calcElementOnMatrixFromRawPatrix x_y_pindex p y of
                            PossibleCase int -> PossibleCase (int + 1)
                            ImpossibleCase -> ImpossibleCase
                        else PossibleCase 0
            Nothing
              ->
                if 0 <= y && y < Array.length y_pindex
                  then ImpossibleCase
                  else PossibleCase 0
      Nothing
        ->
          if 0 <= x && x < Array.length x_y_pindex
            then ImpossibleCase
            else PossibleCase 0

{-| 或る `Patrix` の共終タイプを計算します。
-}
calcCoftypeOfPatrix : Patrix -> Coftype
calcCoftypeOfPatrix patrix
  =
    case patrix of
      Patrix x y x_y_pindex
        ->
          case Array.get (Array.length x_y_pindex - 1) x_y_pindex of
            Just y_pindex
              ->
                let
                  func pindex
                    =
                      case pindex of
                        Null -> True
                        Pindex int
                          ->
                            if 0 <= int && int < Array.length x_y_pindex - 1
                              then False
                              else True
                in
                  if Array.all func y_pindex
                    then One
                    else Omega
            Nothing -> Zero

{-| 或るパトリックスの悪根を計算します。

共終タイプが ω ではない時は、 `Nothing` を返します。
-}
calcBadRootOfPatrix : Patrix -> Maybe (Int, Int)
calcBadRootOfPatrix patrix
  =
    case patrix of
      Patrix x y x_y_pindex
        ->
          case Array.get (Array.length x_y_pindex - 1) x_y_pindex of
            Just y_pindex
              ->
                let
                  func pindex (i, r)
                    =
                      case pindex of
                        Null -> (i + 1, r)
                        Pindex int
                          ->
                            if 0 <= int && int < Array.length x_y_pindex - 1
                              then (i + 1, Just (int, i))
                              else (i + 1, r)
                in
                  case Array.foldl func (0, Nothing) y_pindex of
                    (i, r) -> r
            Nothing -> Nothing

{-| 或るパトリックスを或る係数で展開します。 `Just` で包んだ結果を返します。其の自然数が其の行列の共終タイプ以上なら `Nothing` を返します。
-}
expandPatrix : Patrix -> Nat -> Case (Result (OutOfIndexError Patrix) Patrix)
expandPatrix patrix n
  =
    case calcCoftypeOfPatrix patrix of
      Zero -> PossibleCase (Err (OutOfIndexError patrix n Zero))
      One
        ->
          if 0 <= toIntFromNat n
            then PossibleCase (Err (OutOfIndexError patrix n One))
            else
              case compareNat One n of
                LT -> PossibleCase (Err (OutOfIndexError patrix n One))
                EQ -> PossibleCase (Err (OutOfIndexError patrix n One))
                GT
                  ->
                    case patrix of
                      Patrix x y x_y_pindex -> PossibleCase (Ok (Patrix (x - 1) y (Array.pop x_y_pindex)))
      Omega
        ->
          if 0 <= toIntFromNat n
            then
              case calcBadRootOfPatrix patrix of
                Just (xr, yr)
                  ->
                    case patrix of
                      Patrix x y x_y_pindex
                        ->
                          let
                            ex = xr + (((x - 1) - xr) * (toIntFromNat n + 1))
                          in
                            case expandPatrix_helper_1 x y x_y_pindex n xr yr ex of
                              PossibleCase x_y_pindex_ -> PossibleCase (Ok (Patrix ex y x_y_pindex_))
                              ImpossibleCase -> ImpossibleCase
                Nothing -> ImpossibleCase
            else PossibleCase (Err (OutOfIndexError patrix n Omega))

expandPatrix_helper_1 : Int -> Int -> RawPatrix -> Nat -> Int -> Int -> Int -> Case RawPatrix
expandPatrix_helper_1 x y x_y_pindex n xr yr ex
  =
    let
      pindex_ x_ y_ = expandPatrix_helper_2 x_y_pindex xr yr x_ y_
      y_pindex_ x_ = Case.initializeArrayWithCase y (\y_ -> pindex_ x_ y_)
      x_y_pindex_ = Case.initializeArrayWithCase ex (\x_ -> y_pindex_ x_)
    in
      x_y_pindex_

expandPatrix_helper_2 : RawPatrix -> Int -> Int -> Int -> Int -> Case Pindex
expandPatrix_helper_2 x_y_pindex xr yr x_ y_
  =
    if x_ < xr
      then
        case Array.get x_ x_y_pindex of
          Just y_pindex
            ->
              case Array.get y_ y_pindex of
                Just pindex -> PossibleCase pindex
                Nothing
                  ->
                    if 0 <= y_
                      then
                        if y_ < Array.length y_pindex
                          then ImpossibleCase
                          else PossibleCase Null
                      else
                        if x_ == 0
                          then PossibleCase Null
                          else PossibleCase (Pindex (x_ - 1))
          Nothing -> ImpossibleCase
      else
        let
          x = Array.length x_y_pindex
          m = (x_ - xr) // ((x - 1) - xr)
          n = (x_ - xr) |> modBy ((x - 1) - xr)
        in
          if m == 0
            then
              case Array.get x_ x_y_pindex of
                Just y_pindex
                  ->
                    case Array.get y_ y_pindex of
                      Just pindex -> PossibleCase pindex
                      Nothing
                        ->
                          if 0 <= y_
                            then
                              if y_ < Array.length y_pindex
                                then ImpossibleCase
                                else PossibleCase Null
                            else
                              if x_ == 0
                                then PossibleCase Null
                                else PossibleCase (Pindex (x_ - 1))
                Nothing -> ImpossibleCase
            else
              if y_ < yr
                then
                  if n == 0
                    then
                      case Array.get (x - 1) x_y_pindex of
                        Just y_pindex
                          ->
                            case Array.get y_ y_pindex of
                              Just pindex
                                ->
                                  case pindex of
                                    Null -> PossibleCase Null
                                    Pindex int -> PossibleCase (Pindex (int + ((x - 1) - xr) * (m - 1)))
                              Nothing -> ImpossibleCase
                        Nothing -> ImpossibleCase
                    else
                      case Array.get (xr + n) x_y_pindex of
                        Just y_pindex
                          ->
                            case Array.get y_ y_pindex of
                              Just pindex
                                ->
                                  case pindex of
                                    Null -> PossibleCase Null
                                    Pindex int -> PossibleCase (Pindex (int + ((x - 1) - xr) * m))
                              Nothing -> ImpossibleCase
                        Nothing -> ImpossibleCase
                else
                  if n == 0
                    then
                      case Array.get xr x_y_pindex of
                        Just y_pindex
                          ->
                            case Array.get y_ y_pindex of
                              Just pindex
                                ->
                                  case pindex of
                                    Null -> PossibleCase Null
                                    Pindex int -> PossibleCase (Pindex int)
                              Nothing
                                ->
                                  if 0 <= y_
                                    then
                                      if y_ < Array.length y_pindex
                                        then ImpossibleCase
                                        else PossibleCase Null
                                    else
                                      if x_ == 0
                                        then PossibleCase Null
                                        else PossibleCase (Pindex (x_ - 1))
                        Nothing -> ImpossibleCase
                    else
                      case Array.get (xr + n) x_y_pindex of
                        Just y_pindex
                          ->
                            case Array.get y_ y_pindex of
                              Just pindex
                                ->
                                  case pindex of
                                    Null -> PossibleCase Null
                                    Pindex int -> PossibleCase (Pindex (int + ((x - 1) - xr) * m))
                              Nothing
                                ->
                                  if 0 <= y_
                                    then
                                      if y_ < Array.length y_pindex
                                        then ImpossibleCase
                                        else PossibleCase Null
                                    else
                                      if x_ == 0
                                        then PossibleCase Null
                                        else PossibleCase (Pindex (x_ - 1))
                        Nothing -> ImpossibleCase

notation : Notation (Maxipointed Matrix)
notation
  =
    {
      compare = compareMaxipointed compareMatrix
    ,
      expand
        =
          let
            f nat
              =
                let
                  int = toIntFromNat nat
                in
                  if 0 <= int
                    then Just (Matrix 2 int (Array.fromList [Array.repeat int 0, Array.repeat int 1]))
                    else Nothing
          in
            expandMaxipointed expandMatrix f
    ,
      maximum = Maximum
    }
