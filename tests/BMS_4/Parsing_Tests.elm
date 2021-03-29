module BMS_4.Parsing_Tests exposing (test_fromStringToAst)

import BMS_4.Parsing exposing (..)

import Expect exposing (Expectation)

import Fuzz exposing (Fuzzer)

import Test exposing (..)

test_fromStringToAst : Test
test_fromStringToAst
  =
    describe
      "fromStringToAst"
      [
        test
          "normal case"
          (\_
            ->
              Expect.equal
                (fromStringToAst "(0,0,0)(1,1,1)(2,2,0)")
                (Just [[0, 0, 0], [1, 1, 1], [2, 2, 0]])
        test
          "some spaces and some breaks"
          (\_
            ->
              Expect.equal
                (fromStringToAst
                  """
                  (0, 0, 0)
                  (1, 1, 1)
                  (2, 2, 0)
                  """)
                (Just [[0, 0, 0], [1, 1, 1], [2, 2, 0]])
