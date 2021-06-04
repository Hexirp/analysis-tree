module Main.BMS_4
  exposing
    (
      view
    )

import Array exposing (Array)

import Case exposing (Case (..))
import Notation exposing (..)
import BMS_4
import BMS_4_Printing exposing (..)

import Css exposing (color, rgb)

import Html.Styled exposing (Html, span, text)
import Html.Styled.Attributes exposing (css)

view : Array Int -> Html msg
view x_int
  =
    case Notation.toTermFromRawOuter BMS_4.notation x_int of
      PossibleCase result_result_term
        ->
          case result_result_term of
            Ok result_term
              ->
                case result_term of
                  Ok term
                    ->
                      case term of
                        Lower matrix -> text (printMatrix matrix)
                        Maximum -> text "B"
                  Err (OutOfIndexError term nat coftype)
                    ->
                      span
                        [
                          css
                            [
                              color (rgb 220 20 60)
                            ]
                        ]
                        [
                          text "Error: OutOfIndexError"
                        ]
            Err e
              ->
                span
                  [
                    css
                      [
                        color (rgb 220 20 60)
                      ]
                  ]
                  [
                    text "Fatal Error: An impossible case happened. Please report this error."
                  ]
      ImpossibleCase
        ->
          span
            [
              css
                [
                  color (rgb 220 20 60)
                ]
            ]
            [
              text "Fatal Error: An impossible case happened. Please report this error."
            ]
