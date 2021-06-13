module BMS_4_Printing_Tests
  exposing
    (
      test_print
    )

import Array

import BMS_4 exposing (..)
import BMS_4_Printing exposing (..)

import Expect
import Test exposing (Test, describe, test)

test_print : Test
test_print
  =
    describe "print"
      [
        test_printRawMatrix
      ,
        test_printMatrix
      ]

test_printRawMatrix : Test
test_printRawMatrix
  =
    describe "printRawMatrix"
      [
        let
          expect _
            =
              let
                target = printRawMatrix (Array.fromList [Array.fromList [0, 0], Array.fromList [1, 1], Array.fromList [2, 1]])
                result = "(0,0)(1,1)(2,1)"
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ,
        let
          expect _
            =
              let
                target = printRawMatrix (Array.fromList [])
                result = "ε"
              in
                target |> Expect.equal result
        in
          test "empty matrix" expect
      ]

test_printMatrix : Test
test_printMatrix
  =
    describe "printMatrix"
      [
        let
          expect _
            =
              let
                target = printMatrix (Matrix 3 2 (Array.fromList [Array.fromList [0, 0], Array.fromList [1, 1], Array.fromList [2, 1]]))
                result = "(0,0)(1,1)(2,1)"
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ]
