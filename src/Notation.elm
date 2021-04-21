module Notation exposing (..)

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
