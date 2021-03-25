module Main exposing (Model, Message, initialize, view, update, main)

import Basics exposing (..)

import BMS_4

import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)

import Browser

type alias Model = { content : String }

type Message = Change String | Expand | Clear

initialize : Model
initialize = { content = "" }

view : Model -> Html Message
view model
  =
    div
      [ class "element" ]
      [
        div
          [ class "input-element" ]
          [
            input
              [
                class "input-text-element",
                placeholder "matrix or sequence with parentheses",
                value model.content,
                onInput Change
              ]
              [],
            button
              [ class "input-expand-element", onClick Expand ]
              [ text "Expand" ],
            button
              [ class "input-clear-element", onClick Clear ]
              [ text "Clear" ]
          ],
        div [ class "output-element" ] [ text "This is not implemented." ]
      ]

update : Message -> Model -> Model
update message model
  =
    case message of
      Change s -> { model | content = s }
      _ -> model

main : Program () Model Message
main = Browser.sandbox { init = initialize, view = view, update = update }
