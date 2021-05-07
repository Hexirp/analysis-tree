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
      isGeCoftype
    ,
      IsGeCoftypeError (..)
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
    )

{-| 基本列付きの順序数表記です。

# 自然数
@docs Nat, zero, succ, IsNegativeError, toNatFromInt, toIntFromNat

# 共終タイプ
@docs Coftype, isGeCoftype

# 順序数表記
@docs IsGeCoftypeError, Notation

# 外表記
@docs RawOuter, toRawOuterFromList, toListFromRawOuter, toRawOuterFromTerm, toTermFromRawOuter, Outer, toOuterFromTerm, toTermFromOuter, toOuterFromRawOuter, toRawOuterFromOuter
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

{-| 或る自然数が、或る共終タイプ以上であるかどうか判定します。
-}
isGeCoftype : Nat -> Coftype -> Bool
isGeCoftype nat coftype
  =
    case coftype of
      Zero -> 0 <= toIntFromNat nat
      One -> 1 <= toIntFromNat nat
      Omega -> False

{-| 或る表記の項を或る自然数で展開する時に、其の自然数が大きすぎると発生するエラーです。

たとえば、原始数列システムを順序数の表記として見なしたとき、 `(0,1,0)` を `2` で展開しようとすると、基本列の長さが足りないので、展開できません。そのような時のエラーです。
-}
type IsGeCoftypeError term = IsGeCoftypeError term Nat Coftype

{-| 基本列付きの順序数表記です。

`compare` は、或る二つの項を比較します。これは全順序でなければなりません。

`expand` は、或る項を或る自然数で展開します。其の自然数が其の基本列の長さよりも大きい時は `IsGeCoftypeError` となります。これは `x[n] < x` でなければなりません。これは `x[n] < x[n+1]` でなければなりません。

`maximum` は、表記の限界を表す項です。これが必要なのは、アプリケーションで利用する外表記が、表記の限界を表す項を必要とするからです。これは `a ≤ x → a = x` でなければなりません。

余談として、これは真の順序数表記であることを必要としませんが、もし `compare` が整礎であるならば真の順序数表記になります。
-}
type alias Notation term
  =
    {
      compare : term -> term -> Order
    ,
      expand : term -> Nat -> Case (Result (IsGeCoftypeError term) term)
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

{-| 表記の項から生の外表記の項へ変換します。
-}
toRawOuterFromTerm : Notation term -> term -> Case (Maybe RawOuter)
toRawOuterFromTerm notation term
  =
    case notation.compare term notation.maximum of
      LT -> toRawOuterFromTerm_helper_1 notation term Array.empty notation.maximum
      EQ -> PossibleCase (Just Array.empty)
      GT -> PossibleCase Nothing

toRawOuterFromTerm_helper_1 : Notation term -> term -> Array Int -> term -> Case (Maybe RawOuter)
toRawOuterFromTerm_helper_1 notation term x_int term_
  =
    case notation.expand term_ zero of
      PossibleCase result_term__
        ->
          case result_term__ of
            Ok term__
              ->
                case notation.compare term term__ of
                  LT -> toRawOuterFromTerm_helper_2 notation term x_int term__ 0
                  EQ -> PossibleCase (Just (Array.push 0 x_int))
                  GT -> PossibleCase Nothing
            Err _ -> PossibleCase Nothing
      ImpossibleCase -> ImpossibleCase

toRawOuterFromTerm_helper_2 : Notation term -> term -> Array Int -> term -> Int -> Case (Maybe RawOuter)
toRawOuterFromTerm_helper_2 notation term x_int term_ int
  =
    case toNatFromInt (int + 1) of
      Ok nat
        ->
          case notation.expand term_ nat of
            PossibleCase result_term__
              ->
                case result_term__ of
                  Ok term__
                    ->
                      case notation.compare term term__ of
                        LT -> toRawOuterFromTerm_helper_2 notation term x_int term__ (int + 1)
                        EQ -> PossibleCase (Just (Array.push (int + 1) x_int))
                        GT -> toRawOuterFromTerm_helper_1 notation term (Array.push int x_int) term_
                  Err e
                    ->
                      if 0 <= int
                        then
                          if 1 <= int
                            then toRawOuterFromTerm_helper_1 notation term (Array.push 0 x_int) term_
                            else PossibleCase Nothing
                        else ImpossibleCase
            ImpossibleCase -> ImpossibleCase
      Err e -> ImpossibleCase

{-| 生の外表記の項から表記の項へ変換します。
-}
toTermFromRawOuter : Notation term -> RawOuter -> Case (Result IsNegativeError (Result (IsGeCoftypeError term) term))
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
toOuterFromTerm : Notation term -> term -> Case (Maybe Outer)
toOuterFromTerm notation term
  =
    case toRawOuterFromTerm notation term of
      PossibleCase maybe_x_int
        ->
          case maybe_x_int of
            Just x_int -> PossibleCase (Just (Outer x_int))
            Nothing -> PossibleCase Nothing
      ImpossibleCase -> ImpossibleCase

{-| 外表記の項から表記の項へ変換します。
-}
toTermFromOuter : Notation term -> Outer -> Case (Result IsNegativeError (Result (IsGeCoftypeError term) term))
toTermFromOuter notation outer = toTermFromRawOuter notation (toRawOuterFromOuter outer)

{-| 生の外表記の項から外表記の項へ変換します。
-}
toOuterFromRawOuter : Notation term -> RawOuter -> Case (Result IsNegativeError (Result (IsGeCoftypeError term) (Maybe Outer)))
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
                          PossibleCase maybe_outer -> PossibleCase (Ok (Ok maybe_outer))
                          ImpossibleCase -> ImpossibleCase
                    Err e -> PossibleCase (Ok (Err e))
              Err e -> PossibleCase (Err e)
        ImpossibleCase -> ImpossibleCase

{-| 外表記の項から生の外表記の項へ変換します。
-}
toRawOuterFromOuter : Outer -> RawOuter
toRawOuterFromOuter (Outer x) = x
