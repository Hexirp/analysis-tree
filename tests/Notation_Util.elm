module Notation_Util
  exposing
    (
      fuzzer_Nat
    )

import Notation exposing (..)

import Random
import Shrink
import Fuzz exposing (Fuzzer)

fuzzer_Nat : Fuzzer Nat
fuzzer_Nat
  =
    let
      generator = Random.map Nat (Random.int -100 100)
      shrinker nat
        =
          case nat of
            Nat int -> Shrink.map Nat (Shrink.int <| int)
    in
      Fuzz.custom generator shrinker
