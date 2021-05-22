module Notation_Util
  exposing
    (
      fuzz_nat
    )

import Notation exposing (..)

import Random
import Shrink
import Fuzz exposing (Fuzzer)

fuzz_nat : Fuzzer Nat
fuzz_nat
  =
    let
      generator = Random.map Nat (Random.int -100 100)
      shrinker nat
        =
          case nat of
            Nat int -> Shrink.map Nat (Shrink.int <| int)
    in
      Fuzz.custom generator shrinker
