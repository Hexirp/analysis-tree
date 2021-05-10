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

-- 指定された所が丁度いい所なら追加する。具体的には、
-- 配列の末尾だけ。それ以外は Nothing である。
-- あ、でも、そこが既に指定されている箇所なら何もしない。
-- expandShape では Notation 的に正しくない所にも
-- 木を生やしてしまう可能性があるけど、そこは
-- 表示の所でボタンを無効化することで対応する。
expandShape : Array Int -> Shape -> Maybe Shape
expandShape x_int shape
  =
    case Array.toList x_int of
      [] -> Just shape
      xp :: xs -> expandShape_helper_1 xp xs shape

expandShape_helper_1 : Int -> List Int -> Shape -> Maybe Shape
expandShape_helper_1 xp xs (Shape shape)
  =
    case xs of
      []
        ->
          if 0 <= xp
            then
              if Array.length shape <= xp
                then
                  if Array.length shape < xp
                    then Nothing
                    else Just (Shape (Array.push (Shape Array.empty) shape))
                else Just (Shape shape)
            else Nothing
      xsp :: xss
        ->
          if 0 <= xp
            then
              if Array.length shape <= xp
                then Nothing
                else
                  case Array.get xp shape of
                    Just shape_
                      ->
                        let
                          maybe_shape__ = expandShape_helper_1 xsp xss shape_
                        in
                          case maybe_shape__ of
                            Just shape__ -> Just (Shape (Array.set xp shape__ shape))
                            Nothing -> Nothing
                    Nothing -> Nothing
            else Nothing

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
