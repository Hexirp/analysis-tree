module Notation
  exposing
    (
      Nat
    ,
      toNatFromInt
    ,
      toIntFromNat
    ,
      Coftype (..)
    ,
      isLessThanCoftype
    ,
      IsLessThanCoftypeError (..)
    ,
      Notation (..)
    ,
      RawOuter
    ,
      toMatrixFromRawOuter
    ,
      Outer (..)
    ,
      toOuterFromMatrix
    ,
      toMatrixFromOuter
    ,
      toOuterFromRawOuter
    ,
      toRawOuterFromOuter
    )

import Array exposing (Array)

import Case exposing (Case (..))

{-| これは自然数です。
-}
type Nat = Nat Int

{-| 或る整数から或る自然数へ変換します。
-}
toNatFromInt : Int -> Maybe Nat
toNatFromInt x = if 0 <= x then Just (Nat x) else Nothing

{-| 或る自然数から或る整数へ変換します。
-}
toIntFromNat : Nat -> Int
toIntFromNat (Nat int) = int

{-| これは共終タイプです。
-}
type Coftype = Zero | One | Omega

{-| 或る自然数が、或る共終タイプよりも小さいかどうか判定します。
-}
isLessThanCoftype : Nat -> Coftype -> Bool
isLessThanCoftype nat coftype
  =
    case coftype of
      Zero -> False
      One -> toIntFromNat nat < 1
      Omega -> True

type IsLessThanCoftypeError a = IsLessThanCoftypeError a Nat Coftype

type alias Notation a
  =
    {
      compare : a -> a -> Order
    ,
      expand : a -> Nat -> Case (Result (IsLessThanCoftypeError a) a)
    ,
      maximum : a
    }

{-| 生の外表記です。
-}
type alias RawOuter = Array Int

{-| 生の外表記から表記の項へ変換します。
-}
toTermFromRawOuter : Notation a -> RawOuter -> Maybe (Case (Result (IsLessThanCoftypeError a) a))
toTermFromRawOuter notation outer
  =
    let
      func int maybe_case_result_term
        =
          case toNatFromInt int of
            Just nat
              ->
                case maybe_case_result_term of
                  Just case_result_term
                    ->
                      case case_result_term of
                        PossibleCase result_term
                          ->
                            case result_term of
                              Ok term -> Just (notation.expand term nat)
                              Err e -> Just (PossibleCase (Err e))
                        ImpossibleCase -> Just ImpossibleCase
                  Nothing -> Nothing
            Nothing -> Nothing
    in
      Array.foldl func (Just (PossibleCase (Ok notation.maximum))) outer

{-| 外表記です。
-}
type Outer = Outer RawOuter

{-| 表記の項から外表記へ変換します。
-}
toOuterFromTerm : Notation a -> a -> Case (Maybe Outer)
toOuterFromTerm notation term
  =
    toOuterFromTerm_helper_1 notation term Array.empty notation.maximum

toOuterFromTerm_helper_1 : Notation a -> a -> Array Int -> a -> Case (Maybe Outer)
toOuterFromTerm_helper_1 notation term x_int term_
  =
    case notation.compare term term_ of
      LT
        ->
          toOuterFromTerm_helper_2 notation term x_int term_ 0
      EQ -> PossibleCase (Just (Outer x_int))
      GT -> PossibleCase Nothing

toOuterFromTerm_helper_2 : Notation a -> a -> Array Int -> a -> Int -> Case (Maybe Outer)
toOuterFromTerm_helper_2 notation term x_int term_ int
  =
    case toNatFromInt int of
      Just nat
        ->
          case notation.expand term_ nat of
            PossibleCase result_term__
              ->
                case result_term__ of
                  Ok term__
                    ->
                  Err e
                    ->
                      if 0 <= int
                        then
                          if 1 <= int
                            then toOuterFromTerm_helper_1 notation term (Array.push 0 x_int)
                            else PossibleCase (Just (Outer x_int))
                        else ImpossibleCase
            ImpossibleCase -> ImpossibleCase
      Nothing -> ImpossibleCase

{-| 外表記から行列へ変換します。
-}
toMatrixFromOuter : Outer -> Case (Maybe (Maybe Matrix))
toMatrixFromOuter (Outer outer) = toMatrixFromRawOuter outer

{-| 生の外表記から外表記へ変換します。
-}
toOuterFromRawOuter : RawOuter -> Case (Maybe Outer)
toOuterFromRawOuter x
  =
    case toMatrixFromRawOuter x of
      ImpossibleCase -> ImpossibleCase
      PossibleCase m_m_matrix
        ->
          case m_m_matrix of
            Nothing -> PossibleCase Nothing
            Just m_matrix
              -> toOuterFromMatrix m_matrix

{-| 外表記から生の外表記へ変換します。
-}
toRawOuterFromOuter : Outer -> RawOuter
toRawOuterFromOuter (Outer x) = x
