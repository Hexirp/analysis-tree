module BMS_4Tests exposing (suite)

import Basics exposing (..)

import List exposing (List)

import BMS_4

import Expect exposing (Expectation)

import Fuzz exposing (Fuzzer)

import Test exposing (..)

suite : Test
suite = test "first-test" (\_ -> Expect.equal (List.length []) 0)
