module BMS_4_Tests exposing (test_Matrix, test_Patrix)

import Case exposing (Case (..))

import BMS_4 exposing (..)

import Expect exposing (Expectation)

import Fuzz exposing (Fuzzer)

import Test exposing (..)

test_Matrix : Test
test_Matrix
  = describe "Matrix" [ test_fromArrayToMatrix, test_fromListToMatrix ]

test_fromArrayToMatrix : Test
test_fromArrayToMatrix
  =
    describe "fromArrayToMatrix"
      [
        fuzz
          (Fuzz.array (Fuzz.array Fuzz.int))
          "consisty with fromListToMatrix"
          <|
            \x_y_int
              ->
                fromArrayToMatrix x_y_int
                  |>
                    Expect.equal
                      (fromListToMatrix (fromArrayToList x_y_int)))
      ]

test_fromListToMatrix : Test
test_fromListToMatrix
  =
    describe "fromListToMatrix"
      [
        test "normal case"
          <|
            \_
              ->
                fromListToMatrix [[0,0,0],[1,1,1],[2,2,0]]
                  |>
                    Expect.equal
                      (fromListToMatrixRawly 3 3 [[0,0,0],[1,1,1],[2,2,0]])
      ,
        test "discrete lengths of rows"
          <|
            \_
              ->
                fromListToMatrix [[0],[1,1,1],[2,2]]
                  |>
                    Expect.equal
                      (fromListToMatrixRawly 3 3 [[0,0,0],[1,1,1],[2,2,0]])
      ,
        test "discrete lengths of rows and the non-zero bottom value"
          <|
            \_
              ->
                fromListToMatrix [[-1],[0,0,0],[1,1]]
                  |>
                    Expect.equal
                      (fromListToMatrixRawly 3 3 [[-1,-1,-1],[0,0,0],[1,1,-1]]
      ,
        test "empty rows"
          <|
            \_
              ->
                fromListToMatrix [[],[1,1,1]]
                  |>
                    Expect.equal
                      (fromListToMatrixRawly 2 3 [[0,0,0],[1,1,1]]
      ,
        test "empty rows and the non-zero bottom value"
          <|
            \_
              ->
                fromListToMatrix [[],[0,0,-1]]
                  |>
                    Expect.equal
                      (fromListToMatrixRawly 2 3 [[-1,-1,-1],[0,0,-1]]
      ,
        test "all empty rows"
          <|
            \_
              ->
                fromListToMatrix [[],[],[]]
                  |>
                    Expect.equal
                      (fromListToMatrixRawly 3 0 [[],[],[]])
      ,
        test "the empty colmun"
          <|
            \_
              ->
                fromListToMatrix []
                  |>
                    Expect.equal
                      (fromListToMatrixRawly 0 0 [])
      ]

test_Patrix : Test
test_Patrix
  = describe "Pindex" [ test_calcPatrixFromMatrix ]

test_calcPatrixFromMatrix : Test
test_calcPatrixFromMatrix
  =
    describe "calcPatrixFromMatrix"
      [
        test "normal case"
          <|
            \_
              ->
                calcPatrixFromMatrix
                  (fromListToMatrix
                    [
                      [0,0,0],
                      [1,1,1],
                      [2,2,0]
                    ])
                  |>
                    Expect.equal
                      (PossibleCase
                        (fromListToPatrixRawly
                          3
                          3
                          [
                            [Null, Null, Null],
                            [Pindex 0, Pindex 0, Pindex 0],
                            [Pindex 1, Pindex 1, Null]
                          ]))
      ,
        test "descent sequence"
          <|
            \_
              ->
                calcPatrixFromMatrix
                  (fromListToMatrix
                    [[1],[0]])
                  |>
                    Expect.equal
                      (PossibleCase
                        (fromListToPatrixRawly
                          2
                          1
                          [[Null], [Null]]))
      ]
