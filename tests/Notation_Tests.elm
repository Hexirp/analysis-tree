module Notation_Tests
  exposing
    (
      test_Nat
    ,
      test_Coftype
    )

import Case exposing (Case (..))

import Notation exposing (..)

import Test exposing (Test, describe, test)

import Expect

-- Nat

test_Nat : Test
test_Nat
  =
    describe "Nat"
      [
        test_zero
      ,
        test_succ
      ,
        test_toNatFromInt
      ,
        test_toIntFromNat
      ]

test_zero : Test
test_zero
  =
    describe "zero"
      [
        let
          expect _
            =
              let
                target = zero
                result = Nat 0
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ]
