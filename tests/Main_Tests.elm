module Main_Tests
  exposing
    (
      test_Shape
    )

import Array

import Main exposing (..)

import Expect
import Fuzz
import Test exposing (Test, describe, test)

test_Shape : Test
test_Shape
  =
    describe "Shape"
      [
        test_expandShape
      ,
        test_retractShape
      ]

test_expandShape : Test
test_expandShape
  =
    describe "expandShape"
      [
        let
          expect _
            =
              let
                target = expandShape (Array.fromList [1, 2, 0]) (Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList []), Shape (Array.fromList [])])]))
                result = Just (Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList [])])])]))
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ,
        let
          expect _
            =
              let
                target = expandShape (Array.fromList [1, 1]) (Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [])])]))
                result = Just (Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [])])]))
              in
                target |> Expect.equal result
        in
          test "already exist" expect
      ,
        let
          expect _
            =
              let
                target = expandShape (Array.fromList [1, 2, 0, 0]) (Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [])])]))
                result = Nothing
              in
                target |> Expect.equal result
        in
          test "too deep" expect
      ,
        let
          expect _
            =
              let
                target = expandShape (Array.fromList [1, 4]) (Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [])])]))
                result = Nothing
              in
                target |> Expect.equal result
        in
          test "too big" expect
      ]

test_retractShape : Test
test_retractShape
  =
    describe "retractShape"
      [
        let
          expect _
            =
              let
                target = retractShape (Array.fromList [1, 2]) (Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList []), Shape (Array.fromList [])])]))
                result = Just (Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [])])]))
              in
                target |> Expect.equal result
        in
          test "normal case" expect
      ,
        let
          expect _
            =
              let
                target = retractShape (Array.fromList [1, 4]) (Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [])])]))
                result = Just (Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [])])]))
              in
                target |> Expect.equal result
        in
          test "not exist" expect
      ,
        let
          expect _
            =
              let
                target = retractShape (Array.fromList [1, 1]) (Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList [Shape (Array.fromList []), Shape (Array.fromList []), Shape (Array.fromList [])])]))
                result = Nothing
              in
                target |> Expect.equal result
        in
          test "inner position" expect
      ,
        let
          expect _
            =
              let
                target = retractShape (Array.fromList []) (Shape (Array.fromList []))
                result = Nothing
              in
                target |> Expect.equal result
        in
          test "root" expect
      ]
