module Main exposing (Model, Message, initialize, view, update, main)

import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (placeholder, value)
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
      []
      [
        input
          [
            placeholder "matrix or sequence with parentheses",
            value model.content,
            onInput Change ]
          [],
        button [ onClick Expand ] [ text "Expand" ],
        button [ onClick Clear ] [ text "Clear" ],
        div [] [ text "This is not implemented." ]
      ]

update : Message -> Model -> Model
update message model
  =
    case message of
      Change s -> { model | content = s }
      _ -> model

main : Program () Model Message
main = Browser.sandbox { init = initialize, view = view, update = update }
