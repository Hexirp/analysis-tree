module Main.Notation
  exposing
    (
      view
    )

import Array exposing (Array)

import Case exposing (Case (..))

import Notation exposing (OutOfIndexError (..))

import Notation_Printing exposing (NotationPrintable)

import Css exposing (color, rgb)

import Html.Styled exposing (Html, span, text)
import Html.Styled.Attributes exposing (css)

view : NotationPrintable term -> Array Int -> Html msg
view notation x_int
  =
    case Notation.toTermFromRawOuter { compare = notation.compare, expand = notation.expand, maximum = notation.maximum } x_int of
      PossibleCase result_result_term
        ->
          case result_result_term of
            Ok result_term
              ->
                case result_term of
                  Ok term -> text (notation.print term)
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
