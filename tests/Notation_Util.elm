module Notation_Util
  exposing
    (
      fuzz_nat
    ,
      fuzz_nat_e
    )

import Notation exposing (..)

import Random
import Shrink
import Fuzz exposing (Fuzzer)

fuzz_nat : Fuzzer Nat
fuzz_nat
  =
    let
      generator = Random.map Nat (Random.int 0 15)
      shrinker = Shrink.convert Nat toIntFromNat (Shrink.atLeastInt 0)
    in
      Fuzz.custom generator shrinker

fuzz_nat_e : Fuzzer Nat
fuzz_nat_e
  =
    let
      generator = Random.map Nat (Random.int -16 15)
      shrinker = Shrink.convert Nat toIntFromNat Shrink.int
    in
      Fuzz.custom generator shrinker
