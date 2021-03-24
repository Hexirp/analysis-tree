module Main exposing (Model, Message, initialize, view, update, main)

import Basics exposing (..)

import Array exposing (Array)

import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)

import Browser

{-| Representation of matrixes, the notion on Bashicu Matrix System. -}
type Matrix = Matrix Int Int (Array (Array Int))

{-| Expand a matrix with an integer. Return `Just` the result. Return `Nothing` if the cofinal type is less than the integer. Return `Nothing` if the integer is negative.

  map (expand 2) (fromList [[0,0,0],[1,1,1]]) == map Just (fromList [[0,0],[1,1],[2,2]])
  map (expand 0) (fromList [[0],[1],[2]]) == map Just (fromList [[0],[1]])
  map (expand 2) (fromList [[0],[1],[2]]) == map Just (fromList [[0],[1],[1],[1])
  map (expand 0) (fromList [[0],[0]]) == map Just (fromList [[0],[0]])
  map (expand 1) (fromList [[0],[0]]) == Just Nothing
  map (expand 0) (fromList []) == Just Nothing
  map (expand -1) (fromList [[0],[1],[2]]) == Just Nothing
-}
expand : Int -> Matrix -> Maybe Matrix
expand n x = Just x

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
