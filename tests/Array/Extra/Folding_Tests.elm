module Array.Extra.Folding_Tests
  exposing
    (
      test_Array
    )

import Array exposing (..)
import Array.Extra.Folding exposing (..)

import Expect
import Fuzz
import Test exposing (Test, describe, test, fuzz)

test_Array : Test
test_Array
  =
    describe "Array"
      [
        test_all
      ,
        test_maximum
      ,
        test_minimum
      ]

test_all : Test
test_all
  =
    describe "all"
      [
        let
          expect x_int
            =
              let
                target = all (\x -> 0 <= x) x_int
                result = List.all (\x -> 0 <= x) (Array.toList x_int)
              in
                target |> Expect.equal result
        in
          fuzz (Fuzz.array Fuzz.int) "fuzzing" expect
      ]

test_maximum : Test
test_maximum
  =
    describe "maximum"
      [
        let
          expect x_int
            =
              let
                target = maximum x_int
                result = List.maximum (Array.toList x_int)
              in
                target |> Expect.equal result
        in
          fuzz (Fuzz.array Fuzz.int) "fuzzing" expect
      ]

test_minimum : Test
test_minimum
  =
    describe "minimum"
      [
        let
          expect x_int
            =
              let
                target = minimum x_int
                result = List.minimum (Array.toList x_int)
              in
                target |> Expect.equal result
        in
          fuzz (Fuzz.array Fuzz.int) "fuzzing" expect
      ]
