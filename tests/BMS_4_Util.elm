module BMS_4_Util
  exposing
    (
      fuzz_rawMatrix
    ,
      fuzz_matrix
    ,
      fuzz_pindex
    ,
      fuzz_rawPatrix
    ,
      fuzz_patrix
    )

import BMS_4 exposing (..)

import Random
import Shrink
import Fuzz exposing (Fuzzer)

fuzz_rawMatrix : Fuzzer RawMatrix
fuzz_rawMatrix = Fuzz.array (Fuzz.array Fuzz.int)

fuzz_matrix : Fuzzer Matrix
fuzz_matrix
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

fuzz_pindex : Fuzzer Pindex
fuzz_pindex
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

fuzz_rawPatrix : Fuzzer RawPatrix
fuzz_rawPatrix = Fuzz.array (Fuzz.array fuzz_pindex)

fuzz_patrix : Fuzzer Patrix
fuzz_patrix
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
