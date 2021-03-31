module BMS_4_Tests exposing (test_Matrix, test_Patrix)

import BMS_4 exposing (..)

import Expect exposing (Expectation)

import Fuzz exposing (Fuzzer)

import Test exposing (..)

import Debug

test_Matrix : Test
test_Matrix
  = describe "Matrix" [ test_fromArrayToMatrix, test_fromListToMatrix ]

test_fromArrayToMatrix : Test
test_fromArrayToMatrix
  =
    describe
      "fromArrayToMatrix"
      [
        fuzz
          (Fuzz.array (Fuzz.array Fuzz.int))
          "consisty with fromListToMatrix"
          (\array
            ->
              Expect.equal
                (fromArrayToMatrix array)
                (fromListToMatrix (fromArrayToList array)))
      ]

test_fromListToMatrix : Test
test_fromListToMatrix
  =
    describe
      "fromListToMatrix"
      [
        test
          "normal case"
          (\_
            ->
              Expect.equal
                (fromListToMatrix [[0, 0, 0], [1, 1, 1], [2, 2, 0]])
                (fromListToMatrixRawly 3 3 [[0, 0, 0], [1, 1, 1], [2, 2, 0]])),
        test
          "discrete lengths of rows"
          (\_
            ->
              Expect.equal
                (fromListToMatrix [[0], [1, 1, 1], [2, 2]])
                (fromListToMatrixRawly 3 3 [[0, 0, 0], [1, 1, 1], [2, 2, 0]])),
        test
          "discrete lengths of rows and the non-zero bottom value"
          (\_
            ->
              Expect.equal
                (fromListToMatrix [[-1], [0, 0, 0], [1, 1]])
                (fromListToMatrixRawly
                  3
                  3
                  [[-1, -1, -1], [0, 0, 0], [1, 1, -1]])),
        test
          "empty rows"
          (\_
            ->
              Expect.equal
                (fromListToMatrix [[], [1, 1, 1]])
                (fromListToMatrixRawly 2 3 [[0, 0, 0], [1, 1, 1]])),
        test
          "empty rows and the non-zero bottom value"
          (\_
            ->
              Expect.equal
                (fromListToMatrix [[], [0, 0, -1]])
                (fromListToMatrixRawly 2 3 [[-1, -1, -1], [0, 0, -1]])),
        test
          "all empty rows"
          (\_
            ->
              Expect.equal
                (fromListToMatrix [[], [], []])
                (fromListToMatrixRawly 3 0 [[], [], []])),
        test
          "the empty colmun"
          (\_
            ->
              Expect.equal (fromListToMatrix []) (fromListToMatrixRawly 0 0 []))
      ]

test_Patrix : Test
test_Patrix
  =
    describe
      "Pindex"
      [ test_fromMatrixToPatrix, test_fromMatrixToPatrixWithCatching ]

test_fromMatrixToPatrix : Test
test_fromMatrixToPatrix
  =
    describe
      "fromMatrixToPatrix"
      [
        test
          "normal case"
          (\_
            ->
              Expect.equal
                (fromMatrixToPatrix
                  (fromListToMatrix
                    [
                      [0, 0, 0],
                      [1, 1, 1],
                      [2, 2, 0]
                    ]))
                (fromListToPatrixRawly
                  3
                  3
                  [
                    [Null, Null, Null],
                    [Pindex 0, Pindex 0, Pindex 0],
                    [Pindex 1, Pindex 1, Null]
                  ])),
        test
          "descent sequence"
          (\_
            ->
              Expect.equal
                (fromMatrixToPatrix (fromListToMatrix [[1], [0]]))
                (fromListToPatrixRawly 2 1 [[Null], [Null]]))
      ]

test_fromMatrixToPatrixWithCatching : Test
test_fromMatrixToPatrixWithCatching
  =
    describe
      "fromMatrixToPatrixWithCatching"
      [
        test
          "descent sequence"
          (\_
            ->
              Expect.equal
                (fromMatrixToPatrixWithCatching
                  (fromListToMatrix [[1], [0]])
                  (Maybe.withDefault (Pindex -1))
                (fromListToPatrixRawly 2 1 [[Null], [Null]])),
        fuzz
          (Fuzz.list (Fuzz.list Fuzz.int))
          "safety"
          (\x_y_int
            ->
              Expect.equal
                (fromMatrixToPatrixWithCatching
                  (fromListToMatrix (Debug.log "x_y_int" x_y_int))
                  (\_ -> Debug.todo "impossible case"))
                (fromMatrixToPatrixWithCatching
                  (fromListToMatrix x_y_int)
                  (Maybe.withDefault Null)))
      ]
