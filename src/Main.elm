module Main exposing (Model, Message, initialize, view, update, main)

import Array exposing (Array)

import Dict exposing (Dict)

import Case exposing (Case (..))

import Notation exposing (Notation)

import BMS_4
import BMS_4.Parsing

import Html exposing (Html, div, textarea, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)

import Browser

import Debug

type alias Model
  =
    {
      shape : Shape
    ,
      mapping : Mapping
    ,
      memo : Memo
    }

type Shape = Shape (Array Shape)

initialShape : Shape
initialShape = Shape Array.empty

expandShape : Notation term -> Array Int -> Shape -> Shape
expandShape notation x_int shape
  =
    Debug.todo ""

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

type Message = Edit_Mapping (Array Int) String | Edit_Memo (Array Int) String | Expand (Array Int) | Retract (Array Int)

initialize : Model
initialize = { shape = initialShape, mapping = emptyMapping, memo = emptyMemo }

update : Message -> Model -> Model
update message model = model

view : Model -> Html Message
view model = div [] [ text "TODO!" ]

main : Program () Model Message
main = Browser.sandbox { init = initialize, view = view, update = update }
