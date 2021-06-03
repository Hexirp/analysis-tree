module BMS_4_Tests
  exposing
    (
      test_RawMatrix
    ,
      test_Matrix
    ,
      test_Patrix
    ,
      test_Notation
    )

import Array

import Case exposing (Case (..))
import Notation exposing (Nat (..))
import BMS_4 exposing (..)

import Expect
import Fuzz
import Test exposing (Test, describe, test, fuzz, fuzz2, fuzz3)

import Case_Util exposing (..)
import Notation_Util exposing (..)
import BMS_4_Util exposing (..)

test_RawMatrix : Test
test_RawMatrix = describe "RawMatrix" []

test_Matrix : Test
test_Matrix
  =
    describe "Matrix"
      [
        test_compareMatrix
      ,
        test_toMatrixFromRawMatrix
      ,
        test_toRawMatrixFromMatrix
      ,
        test_expandMatrix
      ]

test_compareMatrix : Test
test_compareMatrix
  =
    describe "compareMatrix"
      [
        let
          expect _
            =
              let
                target
                  =
                    let
                      lhs = Matrix 5 2 (Array.fromList [Array.fromList [0, 0], Array.fromList [1, 1], Array.fromList [2, 0], Array.fromList [3, 1], Array.fromList [1, 1]])
                      rhs = Matrix 2 0 (Array.fromList [Array.fromList [], Array.fromList []])
                    in
                      compareMatrix lhs rhs
                result = GT
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ,
        let
          expect _
            =
              let
                target
                  =
                    let
                      lhs = Matrix 5 2 (Array.fromList [Array.fromList [0, 0], Array.fromList [1, 1], Array.fromList [2, 0], Array.fromList [3, 1], Array.fromList [1, 1]])
                      rhs = Matrix 1 3 (Array.fromList [Array.fromList [0, 0, 0]])
                    in
                      compareMatrix lhs rhs
                result = GT
              in
                target |> Expect.equal result
        in
          test "abnormal case" expect
      ]

test_toMatrixFromRawMatrix : Test
test_toMatrixFromRawMatrix
  =
    describe "toMatrixFromRawMatrix"
      [
        let
          expect _
            =
              let
                target = toMatrixFromRawMatrix (Array.fromList [Array.fromList [0, 0, 0], Array.fromList [1, 1, 1], Array.fromList [2, 2, 0]])
                result = Matrix 3 3 (Array.fromList [Array.fromList [0, 0, 0], Array.fromList [1, 1, 1], Array.fromList [2, 2, 0]])
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ,
        let
          expect _
            =
              let
                target = toMatrixFromRawMatrix (Array.fromList [Array.fromList [0], Array.fromList [1, 1, 1], Array.fromList [2, 2]])
                result = Matrix 3 3 (Array.fromList [Array.fromList [0, 0, 0], Array.fromList [1, 1, 1], Array.fromList [2, 2, 0]])
              in
                target |> Expect.equal result
        in
          test "discrete lengths of rows" expect
      ,
        let
          expect _
            =
              let
                target = toMatrixFromRawMatrix (Array.fromList [Array.fromList [-1], Array.fromList [0, 0, 0], Array.fromList [1, 1]])
                result = Matrix 3 3 (Array.fromList [Array.fromList [0, 0, 0], Array.fromList [1, 1, 1], Array.fromList [2, 2, 0]])
              in
                target |> Expect.equal result
        in
          test "discrete lengths of rows and the non-zero bottom value" expect
      ,
        let
          expect _
            =
              let
                target = toMatrixFromRawMatrix (Array.fromList [Array.fromList [], Array.fromList [1, 1, 1], Array.fromList []])
                result = Matrix 3 3 (Array.fromList [Array.fromList [0, 0, 0], Array.fromList [1, 1, 1], Array.fromList [0, 0, 0]])
              in
                target |> Expect.equal result
        in
          test "empty rows" expect
      ,
        let
          expect _
            =
              let
                target = toMatrixFromRawMatrix (Array.fromList [Array.fromList [0, 0, 0], Array.fromList [1, 1, 0], Array.fromList [2, 2, 0]])
                result = Matrix 3 2 (Array.fromList [Array.fromList [0, 0], Array.fromList [1, 1], Array.fromList [2, 2]])
              in
                target |> Expect.equal result
        in
          test "column truncation" expect
      ,
        let
          expect _
            =
              let
                target = toMatrixFromRawMatrix (Array.fromList [Array.fromList [0], Array.fromList [0, 0], Array.fromList [0, 0, 0]])
                result = Matrix 3 0 (Array.fromList [Array.fromList [], Array.fromList [], Array.fromList []])
              in
                target |> Expect.equal result
        in
          test "all zero rows" expect
      ,
        let
          expect _
            =
              let
                target = toMatrixFromRawMatrix (Array.fromList [Array.fromList [], Array.fromList [0, 0, -1]])
                result = Matrix 2 2 (Array.fromList [Array.fromList [0, 0], Array.fromList [1, 1]])
              in
                target |> Expect.equal result
        in
          test "empty rows and the non-zero bottom value" expect
      ,
        let
          expect _
            =
              let
                target = toMatrixFromRawMatrix (Array.fromList [Array.fromList [], Array.fromList [], Array.fromList []])
                result = Matrix 3 0 (Array.fromList [Array.fromList [], Array.fromList [], Array.fromList []])
              in
                target |> Expect.equal result
        in
          test "all empty rows" expect
      ,
        let
          expect _
            =
              let
                target = toMatrixFromRawMatrix (Array.fromList [])
                result = Matrix 0 0 (Array.fromList [])
              in
                target |> Expect.equal result

        in
          test "the empty colmun" expect
      ]

test_toRawMatrixFromMatrix : Test
test_toRawMatrixFromMatrix
  =
    describe "toRawMatrixFromMatrix"
      [
        let
          expect _
            =
              let
                target = toRawMatrixFromMatrix (toMatrixFromRawMatrix (toRawMatrixFromList [[0, 0], [1, 1], [2, 1]]))
                result = (toRawMatrixFromList [[0, 0], [1, 1], [2, 1]])
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ]

test_expandMatrix : Test
test_expandMatrix
  =
    describe "expandMatrix"
      [
        let
          expect _
            =
              let
                target = expandMatrix (Matrix 5 3 (toRawMatrixFromList [[0, 0, 0], [1, 1, 1], [2, 2, 2], [3, 3, 3], [4, 2, 0]])) (Nat 4)
                result = PossibleCase (Ok (Matrix 16 3 (toRawMatrixFromList [[0, 0, 0], [1, 1, 1], [2, 2, 2], [3, 3, 3], [4, 1, 1], [5, 2, 2], [6, 3, 3], [7, 1, 1], [8, 2, 2], [9, 3, 3], [10, 1, 1], [11, 2, 2], [12, 3, 3], [13, 1, 1], [14, 2, 2], [15, 3, 3]])))
              in
                target |> Expect.equal result
        in
          test "normal case 1" expect
      ,
        let
          expect _
            =
              let
                target = expandMatrix (Matrix 2 3 (Array.fromList [Array.fromList [0, 0, 0], Array.fromList [1, 1, 1]])) (Nat 0)
                result = PossibleCase (Ok (Matrix 1 0 (Array.fromList [Array.fromList []])))
              in
                target |> Expect.equal result
        in
          test "normal case 2" expect
      ]

test_Patrix : Test
test_Patrix
  =
    describe "Pindex"
      [
        test_calcPatrixFromMatrix
      ,
        test_calcParentOnPatrixFromRawMatrix
      ,
        test_calcAncestorSetOnPatrixFromRawMatrix
      ,
        test_calcMatrixFromPatrix
      ,
        test_calcElementOnMatrixFromRawPatrix
      ,
        test_calcBadRootOfPatrix
      ,
        test_expandPatrix
      ]

test_calcPatrixFromMatrix : Test
test_calcPatrixFromMatrix
  =
    describe "calcPatrixFromMatrix"
      [
        let
          expect _
            =
              let
                target = calcPatrixFromMatrix (toMatrixFromRawMatrix (toRawMatrixFromList [[0, 0, 0], [1, 1, 1], [2, 2, 0]]))
                result = PossibleCase (Patrix 3 3 (toRawPatrixFromList [[Null, Null, Null], [Pindex 0, Pindex 0, Pindex 0], [Pindex 1, Pindex 1, Null]]))
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ,
        let
          expect _
            =
              let
                target = calcPatrixFromMatrix (toMatrixFromRawMatrix (toRawMatrixFromList [[0, 0], [1, 1], [2, 0], [3, 1], [1, 1]]))
                result = PossibleCase (Patrix 5 2 (toRawPatrixFromList [[Null, Null], [Pindex 0, Pindex 0], [Pindex 1, Null], [Pindex 2, Pindex 2], [Pindex 0, Pindex 0]]))
              in
                target |> Expect.equal result
        in
          test "crossing neck" expect
      ,
        let
          expect _
            =
              let
                target = calcPatrixFromMatrix (toMatrixFromRawMatrix (toRawMatrixFromList [[0, 0, 0], [1, 1, 1], [2, 2, 2], [3, 3, 3], [4, 2, 0]]))
                result = PossibleCase (Patrix 5 3 (toRawPatrixFromList [[Null, Null, Null], [Pindex 0, Pindex 0, Pindex 0], [Pindex 1, Pindex 1, Pindex 1], [Pindex 2, Pindex 2, Pindex 2], [Pindex 3, Pindex 1, Null]]))
              in
                target |> Expect.equal result
        in
          test "big case" expect
      ,
        let
          expect _
            =
              let
                target = calcPatrixFromMatrix (toMatrixFromRawMatrix (toRawMatrixFromList [[2], [1], [0], [1], [0]]))
                result = PossibleCase (Patrix 5 1 (toRawPatrixFromList [[Null], [Null], [Null], [Pindex 2], [Null]]))
              in
                target |> Expect.equal result
        in
          test "non-ascending sequence" expect
      ,
        let
          expect matrix
            =
              let
                target = calcPatrixFromMatrix matrix
              in
                target |> expect_notImpossibleCase
        in
          fuzz fuzz_matrix_e "follow the rule of the type `Case`" expect
      ]

test_calcParentOnPatrixFromRawMatrix : Test
test_calcParentOnPatrixFromRawMatrix
  =
    describe "calcParentOnPatrixFromRawMatrix"
      [
        let
          expect _
            =
              let
                target = calcParentOnPatrixFromRawMatrix (toRawMatrixFromList [[], [], [0]]) 2 0
                result = PossibleCase (Pindex 1)
              in
                target |> Expect.equal result
        in
          test "almost empty matrix" expect
      ,
        let
          expect x_y_int x y
            =
              let
                target = calcParentOnPatrixFromRawMatrix x_y_int x y
              in
                target |> expect_notImpossibleCase
        in
        fuzz3 fuzz_rawMatrix Fuzz.int Fuzz.int "follow the rule of the type `Case`" expect
      ]

test_calcAncestorSetOnPatrixFromRawMatrix : Test
test_calcAncestorSetOnPatrixFromRawMatrix
  =
    describe "calcAncestorSetOnPatrixFromRawMatrix"
      [
        let
          expect _
            =
              let
                target = calcAncestorSetOnPatrixFromRawMatrix (toRawMatrixFromList [[], [], [0]]) 2 0
              in
                target |> expect_notImpossibleCase
        in
          test "almost empty matrix" expect
      ,
        let
          expect x_y_int x y
            =
              let
                target = calcAncestorSetOnPatrixFromRawMatrix x_y_int x y
              in
                target |> expect_notImpossibleCase
        in
          fuzz3 fuzz_rawMatrix Fuzz.int Fuzz.int "follow the rule of the type `Case`" expect
      ]

test_calcMatrixFromPatrix : Test
test_calcMatrixFromPatrix
  =
    describe "calcMatrixFromPatrix"
      [
        let
          expect patrix
            =
              let
                target = calcMatrixFromPatrix patrix
              in
                target |> expect_notImpossibleCase

        in
          fuzz fuzz_patrix_e "follow the rule of the type `Case`" expect
      ]

test_calcElementOnMatrixFromRawPatrix : Test
test_calcElementOnMatrixFromRawPatrix
  =
    describe "calcElementOnMatrixFromRawPatrix"
      [
        let
          expect x_y_pindex x y
            =
              let
                target = calcElementOnMatrixFromRawPatrix x_y_pindex x y
              in
                target |> expect_notImpossibleCase
        in
          fuzz3 fuzz_rawPatrix Fuzz.int Fuzz.int "follow the rule of the type `Case`" expect
      ]

test_calcBadRootOfPatrix : Test
test_calcBadRootOfPatrix
  =
    describe "calcBadRootOfPatrix"
      [
        let
          expect _
            =
              let
                target = calcBadRootOfPatrix (Patrix 5 3 (toRawPatrixFromList [[Null, Null, Null], [Pindex 0, Pindex 0, Pindex 0], [Pindex 1, Pindex 1, Pindex 1], [Pindex 2, Pindex 2, Pindex 2], [Pindex 3, Pindex 1, Null]]))
                result = Just (1, 1)
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ]

test_expandPatrix : Test
test_expandPatrix
  =
    describe "expandPatrix"
      [
        let
          expect patrix nat
            =
              let
                target = expandPatrix patrix nat
              in
                target |> expect_notImpossibleCase
        in
          fuzz2 fuzz_patrix_e fuzz_nat_e "follow the rule of the type `Case`" expect
      ]

test_Notation : Test
test_Notation
  =
    describe "Notation"
      [
        test_toRawOuterFromTerm
      ]

test_toRawOuterFromTerm : Test
test_toRawOuterFromTerm
  =
    describe "toRawOuterFromTerm"
      [
        let
          expect _
            =
              let
                target = Notation.toRawOuterFromTerm notation (Notation.Lower (toMatrixFromRawMatrix (toRawMatrixFromList [[0, 0], [1, 1], [2, 0], [3, 1], [1, 1]])))
                result = PossibleCase (Ok (Notation.toRawOuterFromList [3, 2, 1, 2, 0, 1, 1, 0, 1, 0, 1, 0, 0]))
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ]
