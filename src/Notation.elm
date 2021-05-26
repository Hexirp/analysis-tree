module Notation
  exposing
    (
      Nat (..)
    ,
      zero
    ,
      succ
    ,
      IsNegativeError (..)
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
      Expander
    ,
      Notation
    ,
      RawOuter
    ,
      toRawOuterFromList
    ,
      toListFromRawOuter
    ,
      toRawOuterFromTerm
    ,
      toTermFromRawOuter
    ,
      Outer (..)
    ,
      toOuterFromTerm
    ,
      toTermFromOuter
    ,
      toOuterFromRawOuter
    ,
      toRawOuterFromOuter
    ,
      canonicalize
    ,
      Maxipointed (..)
    ,
      compareMaxipointed
    ,
      expandMaxipointed
    )

{-| 基本列付きの順序数表記です。

# 自然数
@docs Nat, zero, succ, IsNegativeError, toNatFromInt, toIntFromNat

# 共終タイプ
@docs Coftype, compareNat

# 順序数表記
@docs OutOfIndexError, Expander, Notation

# 外表記
@docs RawOuter, toRawOuterFromList, toListFromRawOuter, toRawOuterFromTerm, toTermFromRawOuter, Outer, toOuterFromTerm, toTermFromOuter, toOuterFromRawOuter, toRawOuterFromOuter, canonicalize

# 最大元の添加
@docs Maxipointed, compareMaxipointed, expandMaxipointed
-}

import Array exposing (Array)

import Case exposing (Case (..))

{-| これは自然数です。
-}
type Nat = Nat Int

{-| 自然数の `0` です。
-}
zero : Nat
zero = Nat 0

{-| 或る自然数を取り、其の後者を返します。
-}
succ : Nat -> Nat
succ (Nat int) = Nat (int + 1)

{-| 或る整数から或る自然数に変換する時に、其の整数が負であると発生するエラーです。
-}
type IsNegativeError = IsNegativeError Int

{-| 或る整数から或る自然数へ変換します。

其の整数が負だと `IsNegativeError` となります。
-}
toNatFromInt : Int -> Result IsNegativeError Nat
toNatFromInt x = if 0 <= x then Ok (Nat x) else Err (IsNegativeError x)

{-| 或る自然数から或る整数へ変換します。
-}
toIntFromNat : Nat -> Int
toIntFromNat (Nat int) = int

{-| これは共終タイプです。
-}
type Coftype = Zero | One | Omega

{-| 或る共終タイプを、或る自然数と比較します。
-}
compareNat : Coftype -> Nat -> Order
compareNat coftype nat
  =
    case coftype of
      Zero -> compare 0 (toIntFromNat nat)
      One -> compare 1 (toIntFromNat nat)
      Omega -> GT

{-| 或る表記の項を或る自然数で展開する時に、其の自然数が大きすぎると発生するエラーです。

たとえば、原始数列システムを順序数の表記として見なしたとき、 `(0,1,0)` を `2` で展開しようとすると、基本列の長さが足りないので、展開できません。そのような時のエラーです。
-}
type OutOfIndexError term = OutOfIndexError term Nat Coftype

{-| 展開関数の型です。
-}
type alias Expander term = term -> Nat -> Case (Result (OutOfIndexError term) term)

{-| 基本列付きの順序数表記です。

`compare` は、或る二つの項を比較します。これは全順序でなければなりません。

`expand` は、或る項を或る自然数で展開します。其の自然数が其の基本列の長さよりも大きい時は `OutOfIndexError` となります。これは `x[n] < x` でなければなりません。これは `x[n] < x[n+1]` でなければなりません。

`maximum` は、表記の限界を表す項です。これが必要なのは、アプリケーションで利用する外表記が、表記の限界を表す項を必要とするからです。これは `a ≤ x → a = x` でなければなりません。

余談として、これは真の順序数表記であることを必要としませんが、もし `compare` が整礎であるならば真の順序数表記になります。
-}
type alias Notation term
  =
    {
      compare : term -> term -> Order
    ,
      expand : Expander term
    ,
      maximum : term
    }

{-| 生の外表記の項です。
-}
type alias RawOuter = Array Int

{-| リストから生の外表記の項へ変換します。
-}
toRawOuterFromList : List Int -> RawOuter
toRawOuterFromList list = Array.fromList list

{-| 生の外表記の項からリストへ変換します。
-}
toListFromRawOuter : RawOuter -> List Int
toListFromRawOuter x_int = Array.toList x_int

{-| 表記の項から生の外表記の項へ変換する時に、それが不可能だと発生するエラーです。

`IsGreaterThanMaximumError` は、其の表記の項が `notation.maximum` より大きいと判定された時に発生するエラーです。

`IsLessThanZeroError` は、其の表記の項が、共終タイプが `Zero` となる項より小さいと判定された時に発生するエラーです。

`IsSkeppedError` は、探索において、其の表記の項に辿り着く前に共終タイプが `One` となる項に辿り着いてしまった時に発生するエラーです。

`IsIrregularSequenceError` は、探索において、共終タイプが `Omega` であるにも関わらず `2` 以上の係数による展開がエラーになる項に辿り着いてしまった時に発生するエラーです。
-}
type IsNotConvertOuterError term = IsGreaterThanMaximumError term (Array Int) term | IsLessThanZeroError term (Array Int) term Nat (OutOfIndexError term) | IsSkeppedError term (Array Int) term Nat (OutOfIndexError term) | IsIrregularSequenceError term (Array Int) term Nat (OutOfIndexError term)

{-| 表記の項から生の外表記の項へ変換します。
-}
toRawOuterFromTerm : Notation term -> term -> Case (Result (IsNotConvertOuterError term) RawOuter)
toRawOuterFromTerm notation term = toRawOuterFromTerm_helper_1 notation term Array.empty notation.maximum

toRawOuterFromTerm_helper_1 : Notation term -> term -> Array Int -> term -> Case (Result (IsNotConvertOuterError term) RawOuter)
toRawOuterFromTerm_helper_1 notation term x_int term_
  =
    case notation.compare term term_ of
      LT -> toRawOuterFromTerm_helper_2 notation term x_int term_ zero
      EQ -> PossibleCase (Ok x_int)
      GT -> PossibleCase (Err (IsGreaterThanMaximumError term x_int term_))

toRawOuterFromTerm_helper_2 : Notation term -> term -> Array Int -> term -> Nat -> Case (Result (IsNotConvertOuterError term) RawOuter)
toRawOuterFromTerm_helper_2 notation term x_int term_ nat
  =
    case notation.expand term_ nat of
      PossibleCase result_term__
        ->
          case result_term__ of
            Ok term__
              ->
                case
                  case notation.compare term term__ of
                    LT -> False
                    EQ -> True
                    GT -> True
                of
                  False -> toRawOuterFromTerm_helper_2 notation term x_int term__ (succ nat)
                  True -> toRawOuterFromTerm_helper_1 notation term (Array.push (toIntFromNat nat) x_int) term__
            Err e
              ->
                if 0 <= toIntFromNat nat
                  then
                    if 1 <= toIntFromNat nat
                      then
                        if 2 <= toIntFromNat nat
                          then PossibleCase (Err (IsIrregularSequenceError term x_int term_ nat e))
                          else PossibleCase (Err (IsSkeppedError term x_int term_ nat e))
                      else PossibleCase (Err (IsLessThanZeroError term x_int term_ nat e))
                  else ImpossibleCase
      ImpossibleCase -> ImpossibleCase

{-| 生の外表記の項から表記の項へ変換します。
-}
toTermFromRawOuter : Notation term -> RawOuter -> Case (Result IsNegativeError (Result (OutOfIndexError term) term))
toTermFromRawOuter notation outer
  =
    let
      func int case_result_result_term
        =
          case toNatFromInt int of
            Ok nat
              ->
                case case_result_result_term of
                  PossibleCase result_result_term
                    ->
                      case result_result_term of
                        Ok result_term
                          ->
                            case result_term of
                              Ok term
                                ->
                                  case notation.expand term nat of
                                    PossibleCase result_term_
                                      ->
                                        case result_term_ of
                                          Ok term_ -> PossibleCase (Ok (Ok term_))
                                          Err e -> PossibleCase (Ok (Err e))
                                    ImpossibleCase -> ImpossibleCase
                              Err e -> PossibleCase (Ok (Err e))
                        Err e -> PossibleCase (Err e)
                  ImpossibleCase -> ImpossibleCase
            Err e -> PossibleCase (Err e)
    in
      Array.foldl func (PossibleCase (Ok (Ok notation.maximum))) outer

{-| 外表記の項です。
-}
type Outer = Outer RawOuter

{-| 表記の項から外表記の項へ変換します。
-}
toOuterFromTerm : Notation term -> term -> Case (Result (IsNotConvertOuterError term) Outer)
toOuterFromTerm notation term
  =
    case toRawOuterFromTerm notation term of
      PossibleCase result_x_int
        ->
          case result_x_int of
            Ok x_int -> PossibleCase (Ok (Outer x_int))
            Err e -> PossibleCase (Err e)
      ImpossibleCase -> ImpossibleCase

{-| 外表記の項から表記の項へ変換します。
-}
toTermFromOuter : Notation term -> Outer -> Case (Result IsNegativeError (Result (OutOfIndexError term) term))
toTermFromOuter notation outer = toTermFromRawOuter notation (toRawOuterFromOuter outer)

{-| 生の外表記の項から外表記の項へ変換します。
-}
toOuterFromRawOuter : Notation term -> RawOuter -> Case (Result IsNegativeError (Result (OutOfIndexError term) (Result (IsNotConvertOuterError term) Outer)))
toOuterFromRawOuter notation x_int
  =
    let
      case_result_result_term = toTermFromRawOuter notation x_int
    in
      case case_result_result_term of
        PossibleCase result_result_term
          ->
            case result_result_term of
              Ok result_term
                ->
                  case result_term of
                    Ok term
                      ->
                        case toOuterFromTerm notation term of
                          PossibleCase result_outer -> PossibleCase (Ok (Ok result_outer))
                          ImpossibleCase -> ImpossibleCase
                    Err e -> PossibleCase (Ok (Err e))
              Err e -> PossibleCase (Err e)
        ImpossibleCase -> ImpossibleCase

{-| 外表記の項から生の外表記の項へ変換します。
-}
toRawOuterFromOuter : Outer -> RawOuter
toRawOuterFromOuter (Outer x) = x

{-| 外表記の項を正規化します。
-}
canonicalize : Notation term -> Outer -> Case (Result IsNegativeError (Result (OutOfIndexError term) (Result (IsNotConvertOuterError term) Outer)))
canonicalize notation outer = toOuterFromRawOuter notation (toRawOuterFromOuter outer)

{-| 最大元が加えられた表記です。
-}
type Maxipointed a =  Lower a | Maximum

{-| `Maxipointed` 型の比較関数を或る元々の表記の比較関数から作ります。
-}
compareMaxipointed : (a -> a -> Order) -> Maxipointed a -> Maxipointed a -> Order
compareMaxipointed f m_x m_y
  =
    case m_x of
      Lower x
        ->
          case m_y of
            Lower y -> f x y
            Maximum -> LT
      Maximum
        ->
          case m_y of
            Lower y -> GT
            Maximum -> EQ

{-| `Maxipointed` 型の展開関数を或る元々の表記の展開関数から作ります。

一つ目の引数は、元々の表記における展開関数です。二つ目の引数は、 `Maximum` の基本列を与える関数です。
-}
expandMaxipointed : Expander a -> (Nat -> Maybe a) -> Expander (Maxipointed a)
expandMaxipointed f g m_term nat
  =
    case m_term of
      Lower term
        ->
          case f term nat of
            PossibleCase result_term_
              ->
                case result_term_ of
                  Ok term_ -> PossibleCase (Ok (Lower term))
                  Err (OutOfIndexError term_ nat_ coftype) -> PossibleCase (Err (OutOfIndexError (Lower term_) nat_ coftype))
            ImpossibleCase -> ImpossibleCase
      Maximum
        ->
          case g nat of
            Just term_ -> PossibleCase (Ok (Lower term_))
            Nothing -> PossibleCase (Err (OutOfIndexError Maximum nat Omega))
