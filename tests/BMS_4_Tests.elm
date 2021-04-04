module BMS_4_Tests exposing (test_collection, test_Matrix, test_Patrix)

import Case exposing (Case (..))

import BMS_4 exposing (..)

import Expect exposing (Expectation)

import Fuzz exposing (Fuzzer)

import Test exposing (Test, describe, test, fuzz, fuzz2, fuzz3)

expect_notImpossibleCase : Case a -> Expectation
expect_notImpossibleCase = Expect.notEqual ImpossibleCase

fuzzer_matrix : Fuzzer Matrix
fuzzer_matrix = Fuzz.map fromListToMatrix (Fuzz.list (Fuzz.list Fuzz.int))

fuzzer_pindex : Fuzzer Pindex
fuzzer_pindex =
  let
    fromMaybeToPindex maybe_x
      =
        case maybe_x of
          Nothing -> Null
          Just x -> Pindex x
  in
    Fuzz.map fromMaybeToPindex (Fuzz.maybe Fuzz.int)

fuzzer_case_patrix : Fuzzer (Case Patrix)
fuzzer_case_patrix = Fuzz.map calcPatrixFromMatrix fuzzer_matrix

test_collection : Test
test_collection
  =
    describe "collection" [ test_fromArrayToList, test_fromListToArray ]

test_fromArrayToList : Test
test_fromArrayToList
  =
    describe "fromArrayToList"
      [
        fuzz
          (Fuzz.array (Fuzz.array Fuzz.int))
          "consisty with fromListToArray"
          <|
            \x_y_int
              ->
                (x_y_int |> fromArrayToList |> fromListToArray)
                  |> Expect.equal x_y_int
      ]

test_fromListToArray : Test
test_fromListToArray
  =
    describe "fromListToArray"
      [
        fuzz
          (Fuzz.list (Fuzz.list Fuzz.int))
          "consisty with fromArrayToList"
          <|
            \x_y_int
              ->
                (x_y_int |> fromListToArray |> fromArrayToList)
                  |> Expect.equal x_y_int
      ]

test_Matrix : Test
test_Matrix
  =
    describe "Matrix"
      [
        test_fromMatrixToArray,
        test_fromArrayToMatrix,
        test_fromMatrixToList,
        test_fromListToMatrix
      ]

test_fromMatrixToArray : Test
test_fromMatrixToArray
  =
    describe "fromMatrixToArray"
      [
        fuzz
          fuzzer_matrix
          "consisty with fromMatrixToList"
          <|
            \matrix
              ->
                (matrix |> fromMatrixToArray |> fromArrayToList)
                  |>
                    Expect.equal
                      (fromMatrixToList matrix)
      ]


test_fromArrayToMatrix : Test
test_fromArrayToMatrix
  =
    describe "fromArrayToMatrix"
      [
        fuzz
          (Fuzz.list (Fuzz.list Fuzz.int))
          "consisty with fromListToMatrix"
          <|
            \x_y_int
              ->
                (x_y_int |> fromListToArray |> fromArrayToMatrix)
                  |>
                    Expect.equal
                      (fromListToMatrix x_y_int)
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
                      (fromListToMatrixRawly 3 3 [[-1,-1,-1],[0,0,0],[1,1,-1]])
      ,
        test "empty rows"
          <|
            \_
              ->
                fromListToMatrix [[],[1,1,1]]
                  |>
                    Expect.equal
                      (fromListToMatrixRawly 2 3 [[0,0,0],[1,1,1]])
      ,
        test "empty rows and the non-zero bottom value"
          <|
            \_
              ->
                fromListToMatrix [[],[0,0,-1]]
                  |>
                    Expect.equal
                      (fromListToMatrixRawly 2 3 [[-1,-1,-1],[0,0,-1]])
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

test_fromMatrixToList : Test
test_fromMatrixToList
  =
    describe "fromMatrixToList"
      [
        test "normal case"
          <|
            \_
              ->
                fromMatrixToList (fromListToMatrix [[0,0],[1,1],[2,1]])
                  |>
                    Expect.equal [[0,0],[1,1],[2,1]]
      ]

test_Patrix : Test
test_Patrix
  =
    describe
      "Pindex"
      [
        test_calcPatrixFromMatrix,
        test_calcParentOnPatrixFromRawMatrix,
        test_calcAncestorSetOnPatrixFromRawMatrix,
        test_calcMatrixFromPatrix,
        test_calcElementOnMatrixFromRawPatrix
      ]

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
                      [0, 0, 0],
                      [1, 1, 1],
                      [2, 2, 0]
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
        test "crossing neck"
          <|
            \_
              ->
                calcPatrixFromMatrix
                  (fromListToMatrix
                    [
                      [0, 0],
                      [1, 1],
                      [2, 0],
                      [3, 1],
                      [1, 1]
                    ])
                  |>
                    Expect.equal
                      (PossibleCase
                        (fromListToPatrixRawly
                          5
                          2
                          [
                            [Null, Null],
                            [Pindex 0, Pindex 0],
                            [Pindex 1, Null],
                            [Pindex 2, Pindex 2],
                            [Pindex 0, Pindex 0]
                          ]))
      ,
        test "non-ascending sequence"
          <|
            \_
              ->
                calcPatrixFromMatrix
                  (fromListToMatrix
                    [[2],[1],[0],[1],[0]])
                  |>
                    Expect.equal
                      (PossibleCase
                        (fromListToPatrixRawly
                          5
                          1
                          [[Null], [Null], [Null], [Pindex 2], [Null]]))
      ,
        fuzz
          fuzzer_matrix
          "follow the rule of the type `Case`"
          <|
            \matrix
              ->
                calcPatrixFromMatrix matrix
                  |>
                    expect_notImpossibleCase
      ]

test_calcParentOnPatrixFromRawMatrix : Test
test_calcParentOnPatrixFromRawMatrix
  =
    describe "calcParentOnPatrixFromRawMatrix"
      [
        fuzz3
          (Fuzz.array (Fuzz.array Fuzz.int))
          Fuzz.int
          Fuzz.int
          "follow the rule of the type `Case`"
          <|
            \x_y_int x y
              ->
                calcParentOnPatrixFromRawMatrix x_y_int x y
                  |>
                    expect_notImpossibleCase
      ]

test_calcAncestorSetOnPatrixFromRawMatrix : Test
test_calcAncestorSetOnPatrixFromRawMatrix
  =
    describe "calcAncestorSetOnPatrixFromRawMatrix"
      [
        fuzz3
          (Fuzz.array (Fuzz.array Fuzz.int))
          Fuzz.int
          Fuzz.int
          "follow the rule of the type `Case`"
          <|
            \x_y_int x y
              ->
                calcAncestorSetOnPatrixFromRawMatrix x_y_int x y
                  |>
                    expect_notImpossibleCase
      ]

test_calcMatrixFromPatrix : Test
test_calcMatrixFromPatrix
  =
    describe "calcMatrixFromPatrix"
      [
        fuzz
          fuzzer_case_patrix
          "follow the rule of the type `Case`"
          <|
            \case_patrix
              ->
                (case case_patrix of
                  ImpossibleCase -> ImpossibleCase
                  PossibleCase patrix -> calcMatrixFromPatrix patrix)
                  |>
                    expect_notImpossibleCase
      ]

test_calcElementOnMatrixFromRawPatrix : Test
test_calcElementOnMatrixFromRawPatrix
  =
    describe "calcElementOnMatrixFromRawPatrix"
      [
        fuzz3
          (Fuzz.array (Fuzz.array fuzzer_pindex))
          Fuzz.int
          Fuzz.int
          "follow the rule of the type `Case`"
          <|
            \x_y_pindex x y
              ->
                calcElementOnMatrixFromRawPatrix x_y_pindex x y
                  |>
                    expect_notImpossibleCase
      ]
