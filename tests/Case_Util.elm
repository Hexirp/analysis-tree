module Case_Util
  exposing
    (
      expect_notImpossibleCase
    )

import Case exposing (..)

import Expect exposing (Expectation)

expect_notImpossibleCase : Case a -> Expectation
expect_notImpossibleCase = Expect.notEqual ImpossibleCase
