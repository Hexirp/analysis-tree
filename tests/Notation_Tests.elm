module Notation_Tests
  exposing
    (
      test_Nat
    )

import Case exposing (Case (..))

import Notation exposing (..)

import Expect
import Fuzz
import Test exposing (Test, describe, test, fuzz)

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

test_succ : Test
test_succ
  =
    describe "succ"
      [
        let
          expect _
            =
              let
                target = succ (Nat 100)
                result = Nat 101
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ,
        let
          expect _
            =
              let
                target = succ (Nat -100)
                result = Nat -99
              in
                target |> Expect.equal result
        in
          test "abnormal case" expect
      ]

test_toNatFromInt : Test
test_toNatFromInt
  =
    describe "toNatFromInt"
      [
        let
          expect _
            =
              let
                target = toNatFromInt 10
                result = Ok (Nat 10)
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ,
        let
          expect _
            =
              let
                target = toNatFromInt -10
                result = Err (IsNegativeError -10)
              in
                target |> Expect.equal result
        in
          test "negative value" expect
      ,
        let
          expect int
            =
              let
                target = toNatFromInt int
                result
                  =
                    if 0 <= int
                      then Ok (Nat int)
                      else Err (IsNegativeError int)
              in
                target |> Expect.equal result
        in
          fuzz Fuzz.int "fuzzing" expect
      ]

test_toIntFromNat : Test
test_toIntFromNat
  =
    describe "toIntFromNat"
      [
        let
          expect _
            =
              let
                target = toIntFromNat (Nat 10)
                result = 10
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ,
        let
          expect _
            =
              let
                target = toIntFromNat (Nat -10)
                result = -10
              in
                target |> Expect.equal result
        in
          test "abnormal case" expect
      ]
