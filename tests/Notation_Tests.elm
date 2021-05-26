module Notation_Tests
  exposing
    (
      test_Nat
    ,
      test_Coftype
    ,
      test_RawOuter
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
      ,
        let
          expect int
            =
              let
                target = toIntFromNat (Nat int)
                result = int
              in
                target |> Expect.equal result
        in
          fuzz Fuzz.int "fuzzing" expect
      ]

test_Coftype : Test
test_Coftype
  =
    describe "Coftype"
      [
        test_compareNat
      ]

test_compareNat : Test
test_compareNat
  =
    describe "compareNat"
      [
        let
          expect _
            =
              let
                target = compareNat Zero (Nat 0)
                result = EQ
              in
                target |> Expect.equal result
        in
          test "normal case with 0 and 0" expect
      ,
        let
          expect _
            =
              let
                target = compareNat Zero (Nat 3)
                result = LT
              in
                target |> Expect.equal result
        in
          test "normal case with 0 and 3" expect
      ,
        let
          expect _
            =
              let
                target = compareNat One (Nat 0)
                result = GT
              in
                target |> Expect.equal result
        in
          test "normal case with 1 and 0" expect
      ,
        let
          expect _
            =
              let
                target = compareNat One (Nat 1)
                result = EQ
              in
                target |> Expect.equal result
        in
          test "normal case with 1 and 1" expect
      ,
        let
          expect _
            =
              let
                target = compareNat One (Nat 3)
                result = LT
              in
                target |> Expect.equal result
        in
          test "normal case with 1 and 3" expect
      ,
        let
          expect _
            =
              let
                target = compareNat Omega (Nat 100)
                result = GT
              in
                target |> Expect.equal result
        in
          test "normal case with ω with 100" expect
      ,
        let
          expect _
            =
              let
                target = compareNat Zero (Nat -1)
                result = GT
              in
                target |> Expect.equal result
        in
          test "abnormal case" expect
      ]

test_RawOuter : Test
test_RawOuter
  =
    describe "RawOuter"
      [
      ]
