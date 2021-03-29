module BMS_4.Parsing_Tests
  exposing (test_parse, test_fromStringToAst, test_fromAstToString)

import Parser

import BMS_4.Parsing exposing (..)

import Expect exposing (Expectation)

import Fuzz exposing (Fuzzer)

import Test exposing (..)

test_parse : Test
test_parse
  =
    describe
      "parse"
      [
        test
          "normal case"
          (\_
            ->
              Expect.ok (Parser.run parse "(0,0,0)(1,1,1)(2,2,0)")),
        test
          "some spaces and some breaks"
          (\_
            ->
              Expect.ok
                (Parser.run parse "(0, 0, 0)\n(1, 1, 1)\n(2, 2, 0)\n"))
      ]

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
                (Just [[0, 0, 0], [1, 1, 1], [2, 2, 0]])),
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
                (Just [[0, 0, 0], [1, 1, 1], [2, 2, 0]]))
      ]

test_fromAstToString : Test
test_fromAstToString
  =
    describe
      "fromAstToString"
      [
        test
          "normal case"
          (\_
            ->
              Expect.equal
                (fromAstToString [[0, 0, 0], [1, 1, 1], [2, 2, 0]])
                "(0,0,0)(1,1,1)(2,2,0)")
      ]
