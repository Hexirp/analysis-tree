module BMS_4Tests exposing (matrix)

import Basics exposing (..)

import List

import BMS_4 exposing (..)

import Expect exposing (Expectation)

import Fuzz exposing (Fuzzer)

import Test exposing (..)

matrix : Test
matrix = describe "Matrix" [ matrixAndArray, matrixAndList ]

matrixAndArray : Test
matrixAndArray
  =
    describe "Matrix and Array"
      [
        fuzz
          (Fuzz.array (Fuzz.array Fuzz.int))
          "consisty with List"
          (\array
            ->
              Expect.equal
                (fromArrayToMatrix array)
                (fromListToMatrix (fromArrayToList array)))
      ]

matrixAndList : Test
matrixAndList
  =
    describe "Matrix and List"
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
