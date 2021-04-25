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
      One
        ->
          case nat of
            Nat int -> int < 1
      Omega -> True

type IsLessThanCoftypeError a = IsLessThanCoftypeError a Nat Coftype

type Notation a
  =
    Notation
      (a -> a -> Order)
      (a -> Nat -> Case (Result (IsLessThanCoftypeError a) a))
      a

{-| バシク行列システム 4 の生の外表記です。
-}
type alias RawOuter = List Int

{-| 生の外表記から行列へ変換します。
-}
toMatrixFromRawOuter : RawOuter -> Case (Maybe (Maybe Matrix))
toMatrixFromRawOuter outer
  =
    let
      f int c_m_m_matrix
        =
          case c_m_m_matrix of
            ImpossibleCase -> ImpossibleCase
            PossibleCase m_m_matrix
              ->
                case m_m_matrix of
                  Nothing -> PossibleCase Nothing
                  Just m_matrix
                    ->
                      case m_matrix of
                        Nothing
                          ->
                            if 0 <= int
                              then
                                PossibleCase
                                  (Just
                                    (Just
                                      (toMatrixFromRawMatrix
                                        (toRawMatrixFromList
                                          [
                                            List.repeat int 0
                                          ,
                                            List.repeat int 1
                                          ]))))
                              else
                                PossibleCase Nothing
                        Just matrix
                          ->
                            case toNatFromInt int of
                              Nothing -> PossibleCase Nothing
                              Just nat
                                ->
                                  case expandMatrix matrix nat of
                                    ImpossibleCase -> ImpossibleCase
                                    PossibleCase m_matrix_
                                      ->
                                        case m_matrix_ of
                                          Nothing -> PossibleCase Nothing
                                          Just matrix_
                                            ->
                                              PossibleCase (Just (Just matrix_))
    in
      List.foldl f (PossibleCase (Just Nothing)) outer

{-| バシク行列システム 4 の外表記です。
-}
type Outer = Outer RawOuter

{-| 行列から外表記へ変換します。
-}
toOuterFromMatrix : Maybe Matrix -> Case (Maybe Outer)
toOuterFromMatrix m_matrix
  =
    case m_matrix of
      Nothing -> PossibleCase (Just (Outer []))
      Just matrix -> toOuterFromMatrix_helper_1 matrix []

toOuterFromMatrix_helper_1 : Matrix -> List Int -> Case (Maybe Outer)
toOuterFromMatrix_helper_1 matrix x
  =
    toOuterFromMatrix_helper_2 matrix x 0

toOuterFromMatrix_helper_2 : Matrix -> List Int -> Int -> Case (Maybe Outer)
toOuterFromMatrix_helper_2 matrix x n
  =
    case toMatrixFromRawOuter (x ++ [n]) of
      ImpossibleCase -> ImpossibleCase
      PossibleCase m_m_matrix_
        ->
          case m_m_matrix_ of
            Nothing
              ->
                if n == 0
                  then PossibleCase Nothing
                  else toOuterFromMatrix_helper_1 matrix (x ++ [n - 1])
            Just m_matrix_
              ->
                case m_matrix_ of
                  Nothing -> ImpossibleCase
                  Just matrix_
                    ->
                      case compareMatrix matrix matrix_ of
                        LT -> toOuterFromMatrix_helper_1 matrix (x ++ [n - 1])
                        EQ -> PossibleCase (Just (Outer (x ++ [n])))
                        GT -> toOuterFromMatrix_helper_2 matrix x (n + 1)

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
