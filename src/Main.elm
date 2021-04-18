module Main exposing (Model, Message, initialize, view, update, main)

import Dict exposing (Dict)

import BMS_4
import BMS_4.Parsing

import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)

import Browser

type Model = Model Shape Mapping Memo

type Shape = Shape (List Shape)

expandShape : List Int -> Shape -> Shape
expandShape x (Shape shape)
  =
    case x of
      [] -> Shape (shape ++ [Shape []])
      xp :: xs
        ->
          Shape (expandShape_helper_1 xp xs shape)

expandShape_helper_1 : Int -> List Int -> List Shape -> List Shape
expandShape_helper_1 xp xs shape
  =
    case shape of
      [] -> []
      shape_p :: shape_s
        ->
          if xp == 0
            then expandShape xs shape_p :: shape_s
            else shape_p :: expandShape_helper_1 (xp - 1) xs shape_s

retractShape : List Int -> Shape -> Shape
retractShape x (Shape shape)
  =
    case x of
      [] -> Shape shape
      xp :: xs
        ->
          Shape (retractShape_helper_1 xp xs shape)

retractShape_helper_1 : Int -> List Int -> List Shape -> List Shape
retractShape_helper_1 xp xs shape
  =
    case shape of
      [] -> []
      shape_p :: shape_s
        ->
          if xp == 0
            then
              case xs of
                []
                  ->
                    case shape_s of
                      [] -> shape_s
                      _ :: _ -> retractShape xs shape_p :: shape_s
                _ :: _ -> retractShape xs shape_p :: shape_s
            else shape_p :: retractShape_helper_1 (xp - 1) xs shape_s

type Mapping = Mapping (Dict (List Int) String)

emptyMapping : Mapping
emptyMapping = Mapping Dict.empty

getMapping : List Int -> Mapping -> Maybe String
getMapping k (Mapping dict) = Dict.get k dict

insertMapping : List Int -> String -> Mapping -> Mapping
insertMapping k v (Mapping dict) = Mapping (Dict.insert k v dict)

type Memo = Memo (Dict (List Int) String)

emptyMemo : Memo
emptyMemo = Memo Dict.empty

getMemo : List Int -> Memo -> Maybe String
getMemo k (Memo dict) = Dict.get k dict

insertMemo : List Int -> String -> Memo -> Memo
insertMemo k v (Memo dict) = Memo (Dict.insert k v dict)

type Message
  =
    Edit_Mapping (List Int) String
      | Edit_Memo (List Int) String
      | Expand (List Int)
      | Retract (List Int)

initialize : Model
initialize = Model (Shape []) emptyMapping emptyMemo

update : Message -> Model -> Model
update message model
  =
    case message of
      Edit_Mapping x s
        ->
          case model of
            Model shape mapping memo
              ->
                Model shape (insertMapping x s mapping) memo
      Edit_Memo x s
        ->
          case model of
            Model shape mapping memo
              ->
                Model shape mapping (insertMemo x s memo)
      Expand x
        ->
          case model of
            Model shape mapping memo
              ->
                Model (expandShape x shape) mapping memo
      Retract x
        ->
          case model of
            Model shape mapping memo
              ->
                Model (retractShape x shape) mapping memo

view : Model -> Html Message
view model
  =
    case model of
      Model shape mapping memo
        ->
          view_helper_1 shape mapping memo []

view_helper_1 : Shape -> Mapping -> Memo -> List Int -> Html Message
view_helper_1 shape mapping memo x
  =
    case shape of
      Shape shape_
        ->
          let
            f i shape__ = view_helper_1 shape__ mapping memo (x ++ [i])
          in
            view_helper_2 mapping memo x (List.indexedMap f shape_)

view_helper_2
  : Mapping -> Memo -> List Int -> List (Html Message) -> Html Message
view_helper_2 mapping memo x nodes
  =
    div [ class "node" ]
      [
        div [ class "node-button" ]
          [
            button
              [
                class "node-button-expand"
              ,
                onClick (Expand x)
              ]
              [
                text "Expand"
              ]
          ,
            button
              [
                class "node-button-retract"
              ,
                onClick (Retract x)
              ]
              [
                text "Retract"
              ]
          ]
      ,
        div
          [
            class "node-children"
          ,
            Html.Attributes.style "padding" "1em"
          ,
            Html.Attributes.style "border" "solid"
          ]
          nodes
      ,
        div [ class "node-input" ]
          [
            div [ class "node-input-mapping" ]
              [
                input
                  [
                    class "node-input-mapping-body"
                  ,
                    onInput (Edit_Mapping x)
                  ]
                  [
                    case getMapping x mapping of
                      Nothing -> text ""
                      Just s -> text s
                  ]
              ]
          ,
            div [ class "node-input-memo" ]
              [
                input
                  [
                    class "node-input-memo-body"
                  ,
                    onInput (Edit_Memo x)
                  ]
                  [
                    case getMemo x memo of
                      Nothing -> text ""
                      Just s -> text s
                  ]
              ]
          ]
      ]

main : Program () Model Message
main = Browser.sandbox { init = initialize, view = view, update = update }
