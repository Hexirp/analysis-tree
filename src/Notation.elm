module Notation
  exposing
    (
      Nat
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
      isLessThanCoftype
    ,
      IsLessThanCoftypeError (..)
    ,
      Notation
    ,
      RawOuter
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

import Array exposing (Array)

import Case exposing (Case (..))

{-| これは自然数です。
-}
type Nat = Nat Int

zero : Nat
zero = Nat 0

succ : Nat -> Nat
succ (Nat int) = Nat (int + 1)

type IsNegativeError = IsNegativeError Int

{-| 或る整数から或る自然数へ変換します。
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

{-| 生の外表記の項です。
-}
type alias RawOuter = Array Int

{-| 表記の項から生の外表記の項へ変換します。
-}
toRawOuterFromTerm : Notation a -> a -> Case (Maybe RawOuter)
toRawOuterFromTerm notation term
  =
    case notation.compare term notation.maximum of
      LT -> toRawOuterFromTerm_helper_1 notation term Array.empty notation.maximum
      EQ -> PossibleCase (Just Array.empty)
      GT -> PossibleCase Nothing

toRawOuterFromTerm_helper_1 : Notation a -> a -> Array Int -> a -> Case (Maybe RawOuter)
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

toRawOuterFromTerm_helper_2 : Notation a -> a -> Array Int -> a -> Int -> Case (Maybe RawOuter)
toRawOuterFromTerm_helper_2 notation term x_int term_ int
  =
    case toNatFromInt (int + 1) of
      Just nat
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
      Nothing -> ImpossibleCase

{-| 生の外表記の項から表記の項へ変換します。
-}
toTermFromRawOuter : Notation a -> RawOuter -> Case (Result IsNegativeError (Result (IsLessThanCoftypeError a) a))
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
toOuterFromTerm : Notation a -> a -> Case (Maybe Outer)
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
toTermFromOuter : Notation a -> Outer -> Case (Result IsNegativeError (Result (IsLessThanCoftypeError a) a))
toTermFromOuter outer = toTermFromRawOuter (toOuterFromRawOuter outer)

{-| 生の外表記の項から外表記の項へ変換します。
-}
toOuterFromRawOuter : Notation a -> RawOuter -> Case (Result IsNegativeError (Result (IsLessThanCoftypeError a) (Maybe a)))
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
