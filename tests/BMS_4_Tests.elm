module BMS_4_Tests exposing (test_Matrix, test_Patrix)

import Case exposing (Case (..))

import BMS_4 exposing (..)

import Random

import Expect exposing (Expectation)

import Shrink

import Fuzz exposing (Fuzzer)

import Test exposing (Test, describe, test, fuzz, fuzz2, fuzz3)

expect_notImpossibleCase : Case a -> Expectation
expect_notImpossibleCase = Expect.notEqual ImpossibleCase

fuzzer_RawMatrix : Fuzzer RawMatrix
fuzzer_RawMatrix = Fuzz.array (Fuzz.array Fuzz.int)

fuzzer_Matrix : Fuzzer Matrix
fuzzer_Matrix
  =
    let
      generator
        =
          Random.map3 Matrix
            (Random.int -100 100)
            (Random.int -100 100)
            (Random.map toRawMatrixFromList
              (Random.list 10
                (Random.list 10
                  (Random.int -100 100))))
      shrinker matrix
        =
          case matrix of
            Matrix x y x_y_int
              ->
                Shrink.map Matrix (Shrink.int <| x)
                  |> Shrink.andMap (Shrink.int <| y)
                  |>
                    Shrink.andMap
                      (Shrink.array (Shrink.array Shrink.int) <| x_y_int)
    in
      Fuzz.custom generator shrinker

fuzzer_Pindex : Fuzzer Pindex
fuzzer_Pindex
  =
    let
      generator
        =
          Random.weighted (1, True) [(10, False)]
            |>
              Random.andThen
                (\bool
                  ->
                    if bool
                      then Random.constant Null
                      else Random.map Pindex (Random.int -100 100))
      shrinker pindex
        =
          case pindex of
            Null -> Shrink.noShrink Null
            Pindex int -> Shrink.map Pindex (Shrink.int int)
    in
      Fuzz.custom generator shrinker

fuzzer_RawPatrix : Fuzzer RawPatrix
fuzzer_RawPatrix = Fuzz.array (Fuzz.array fuzzer_Pindex)

fuzzer_Patrix : Fuzzer Patrix
fuzzer_Patrix
  =
    let
      generator_Pindex
        =
          Random.weighted (1, True) [(10, False)]
            |>
              Random.andThen
                (\bool
                  ->
                    if bool
                      then Random.constant Null
                      else Random.map Pindex (Random.int -100 100))
      shrinker_Pindex pindex
        =
          case pindex of
            Null -> Shrink.noShrink Null
            Pindex int -> Shrink.map Pindex (Shrink.int int)
      generator
        =
          Random.map3 Patrix
            (Random.int -100 100)
            (Random.int -100 100)
            (Random.map toRawPatrixFromList
              (Random.list 10
                (Random.list 10
                  generator_Pindex)))
      shrinker patrix
        =
          case patrix of
            Patrix x y x_y_pindex
              ->
                Shrink.map Patrix (Shrink.int <| x)
                  |> Shrink.andMap (Shrink.int <| y)
                  |>
                    Shrink.andMap
                      (Shrink.array (Shrink.array shrinker_Pindex)
                        <|
                          x_y_pindex)
    in
      Fuzz.custom generator shrinker

test_Matrix : Test
test_Matrix
  =
    describe "Matrix"
      [
        test_toMatrixFromRawMatrix,
        test_toRawMatrixFromMatrix,
        test_expandMatrix
      ]

test_toMatrixFromRawMatrix : Test
test_toMatrixFromRawMatrix
  =
    describe "toMatrixFromRawMatrix"
      [
        test "normal case"
          <|
            \_
              ->
                toMatrixFromRawMatrix
                  (toRawMatrixFromList
                    [[0,0,0],[1,1,1],[2,2,0]])
                  |>
                    Expect.equal
                      (Matrix 3 3
                        (toRawMatrixFromList
                          [[0,0,0],[1,1,1],[2,2,0]]))
      ,
        test "discrete lengths of rows"
          <|
            \_
              ->
                toMatrixFromRawMatrix
                  (toRawMatrixFromList
                    [[0],[1,1,1],[2,2]])
                  |>
                    Expect.equal
                      (Matrix 3 3
                        (toRawMatrixFromList
                          [[0,0,0],[1,1,1],[2,2,0]]))
      ,
        test "discrete lengths of rows and the non-zero bottom value"
          <|
            \_
              ->
                toMatrixFromRawMatrix
                  (toRawMatrixFromList
                    [[-1],[0,0,0],[1,1]])
                  |>
                    Expect.equal
                      (Matrix 3 3
                        (toRawMatrixFromList
                          [[-1,-1,-1],[0,0,0],[1,1,-1]]))
      ,
        test "empty rows"
          <|
            \_
              ->
                toMatrixFromRawMatrix
                  (toRawMatrixFromList
                    [[],[1,1,1]])
                  |>
                    Expect.equal
                      (Matrix 2 3
                        (toRawMatrixFromList
                          [[0,0,0],[1,1,1]]))
      ,
        test "empty rows and the non-zero bottom value"
          <|
            \_
              ->
                toMatrixFromRawMatrix
                  (toRawMatrixFromList
                    [[],[0,0,-1]])
                  |>
                    Expect.equal
                      (Matrix 2 3
                        (toRawMatrixFromList
                          [[-1,-1,-1],[0,0,-1]]))
      ,
        test "all empty rows"
          <|
            \_
              ->
                toMatrixFromRawMatrix
                  (toRawMatrixFromList
                    [[],[],[]])
                  |>
                    Expect.equal
                      (Matrix 3 0
                        (toRawMatrixFromList
                          [[],[],[]]))
      ,
        test "the empty colmun"
          <|
            \_
              ->
                toMatrixFromRawMatrix
                  (toRawMatrixFromList
                    [])
                  |>
                    Expect.equal
                      (Matrix 0 0
                        (toRawMatrixFromList
                          []))
      ]

test_toRawMatrixFromMatrix : Test
test_toRawMatrixFromMatrix
  =
    describe "toRawMatrixFromMatrix"
      [
        test "normal case"
          <|
            \_
              ->
                toRawMatrixFromMatrix
                  (toMatrixFromRawMatrix
                    (toRawMatrixFromList
                      [[0,0],[1,1],[2,1]]))
                  |>
                    Expect.equal
                      (toRawMatrixFromList
                        [[0,0],[1,1],[2,1]])
      ]

test_expandMatrix : Test
test_expandMatrix
  =
    describe "expandMatrix"
      [
        test "normal case"
          <|
            \_
              ->
                expandMatrix
                  (Matrix 5 3
                    (toRawMatrixFromList
                      [
                        [0, 0, 0],
                        [1, 1, 1],
                        [2, 2, 2],
                        [3, 3, 3],
                        [4, 2, 0]
                      ]))
                  (Nat 2)
                  |>
                    Expect.equal
                      (PossibleCase
                        (Just
                          (Matrix 16 3
                            (toRawMatrixFromList
                              [
                                [0, 0, 0],
                                [1, 1, 1],
                                [2, 2, 2],
                                [3, 3, 3],
                                [4, 1, 1],
                                [5, 2, 2],
                                [6, 3, 3],
                                [7, 1, 1],
                                [8, 2, 2],
                                [9, 3, 3],
                                [10, 1, 1],
                                [11, 2, 2],
                                [12, 3, 3],
                                [13, 1, 1],
                                [14, 2, 2],
                                [15, 3, 3]
                            ]))))
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
        test_calcElementOnMatrixFromRawPatrix,
        test_calcBadRootOfPatrix
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
                  (toMatrixFromRawMatrix
                    (toRawMatrixFromList
                      [
                        [0, 0, 0],
                        [1, 1, 1],
                        [2, 2, 0]
                      ]))
                  |>
                    Expect.equal
                      (PossibleCase
                        (Patrix 3 3
                          (toRawPatrixFromList
                            [
                              [Null, Null, Null],
                              [Pindex 0, Pindex 0, Pindex 0],
                              [Pindex 1, Pindex 1, Null]
                            ])))
      ,
        test "crossing neck"
          <|
            \_
              ->
                calcPatrixFromMatrix
                  (toMatrixFromRawMatrix
                    (toRawMatrixFromList
                      [
                        [0, 0],
                        [1, 1],
                        [2, 0],
                        [3, 1],
                        [1, 1]
                      ]))
                  |>
                    Expect.equal
                      (PossibleCase
                        (Patrix 5 2
                          (toRawPatrixFromList
                            [
                              [Null, Null],
                              [Pindex 0, Pindex 0],
                              [Pindex 1, Null],
                              [Pindex 2, Pindex 2],
                              [Pindex 0, Pindex 0]
                            ])))
      ,
        test "big case"
          <|
            \_
              ->
                calcPatrixFromMatrix
                  (toMatrixFromRawMatrix
                    (toRawMatrixFromList
                      [
                        [0, 0, 0],
                        [1, 1, 1],
                        [2, 2, 2],
                        [3, 3, 3],
                        [4, 2, 0]
                      ]))
                  |>
                    Expect.equal
                      (PossibleCase
                        (Patrix 5 3
                          (toRawPatrixFromList
                            [
                              [Null, Null, Null],
                              [Pindex 0, Pindex 0, Pindex 0],
                              [Pindex 1, Pindex 1, Pindex 1],
                              [Pindex 2, Pindex 2, Pindex 2],
                              [Pindex 3, Pindex 1, Null]
                            ])))
      ,
        test "non-ascending sequence"
          <|
            \_
              ->
                calcPatrixFromMatrix
                  (toMatrixFromRawMatrix
                    (toRawMatrixFromList
                      [[2], [1], [0], [1], [0]]))
                  |>
                    Expect.equal
                      (PossibleCase
                        (Patrix 5 1
                          (toRawPatrixFromList
                            [[Null], [Null], [Null], [Pindex 2], [Null]])))
      ,
        fuzz
          fuzzer_Matrix
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
        test "almost empty matrix"
          <|
            \_
              ->
                calcParentOnPatrixFromRawMatrix
                  (toRawMatrixFromList [[], [], [0]])
                  2
                  0
                  |>
                    Expect.equal
                      (PossibleCase (Pindex 1))
      ,
        fuzz3
          fuzzer_RawMatrix
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
        test "almost empty matrix"
          <|
            \_
              ->
                calcAncestorSetOnPatrixFromRawMatrix
                  (toRawMatrixFromList [[], [], [0]])
                  2
                  0
                  |>
                    expect_notImpossibleCase
      ,
        fuzz3
          fuzzer_RawMatrix
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
          fuzzer_Patrix
          "follow the rule of the type `Case`"
          <|
            \patrix
              ->
                calcMatrixFromPatrix patrix
                  |>
                    expect_notImpossibleCase
      ]

test_calcElementOnMatrixFromRawPatrix : Test
test_calcElementOnMatrixFromRawPatrix
  =
    describe "calcElementOnMatrixFromRawPatrix"
      [
        fuzz3
          fuzzer_RawPatrix
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

test_calcBadRootOfPatrix : Test
test_calcBadRootOfPatrix
  =
    describe "calcBadRootOfPatrix"
      [
        test "normal case"
          <|
            \_
              ->
                calcBadRootOfPatrix
                  (Patrix 5 3
                    (toRawPatrixFromList
                      [
                        [Null, Null, Null],
                        [Pindex 0, Pindex 0, Pindex 0],
                        [Pindex 1, Pindex 1, Pindex 1],
                        [Pindex 2, Pindex 2, Pindex 2],
                        [Pindex 3, Pindex 1, Null]
                      ]))
                  |>
                    Expect.equal (Just (1, 1))
      ]
