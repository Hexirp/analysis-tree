module Main.Notation
  exposing
    (
      Mapping (..)
    ,
      Memo (..)
    ,
      Model
    ,
      initialize
    ,
      initializeMapping
    ,
      initializeMemo
    ,
      update
    ,
      updateMapping
    ,
      updateMemo
    ,
      view
    ,
      viewMapping
    ,
      viewMemo
    )

import Dict exposing (Dict)

import Array exposing (Array)

import Css
  exposing
    (
      displayFlex
    ,
      alignItems
    ,
      width
    ,
      height
    ,
      padding
    ,
      padding2
    ,
      backgroundColor
    ,
      borderStyle
    ,
      boxShadow
    ,
      boxShadow4
    ,
      fontSize
    ,
      color
    ,
      center
    ,
      none
    ,
      px
    ,
      rgb
    ,
      hover
    )

import Html.Styled exposing (Html, div, button, textarea, span, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick, onInput)

import Case exposing (Case (..))

import Notation
import Notation_Printing

import Main.Message exposing (Message (..))
import Main.Shape exposing (Shape (..), expandShape, retractShape)

type Mapping = Mapping (Dict (List Int) String)

type Memo = Memo (Dict (List Int) String)

-- 基本的にモデルの操作は単純に。
-- モデルに不整合が出る操作は view で弾く。
-- Model でチェックして、 view でもチェックする必要が
-- あるのなら、 View だけでチェックしたほうが簡単である。
-- ボタンの色を薄くするなどの処理が必要なので、
-- view でもチェックする必要があるんだよねえ……
type alias Model term
  =
    {
      notation : Notation_Printing.NotationPrintable term
    ,
      shape : Shape
    ,
      mapping : Mapping
    ,
      memo : Memo
    }

initialize : Notation_Printing.NotationPrintable term -> Model term
initialize notation
  =
    {
      notation = notation
    ,
      shape = Shape Array.empty
    ,
      mapping = initializeMapping
    ,
      memo = initializeMemo
    }

initializeMapping : Mapping
initializeMapping = Mapping Dict.empty

initializeMemo : Memo
initializeMemo = Memo Dict.empty

update : Message -> Model term -> Model term
update message model
  =
    case message of
      Expand x_int
        ->
          case expandShape x_int model.shape of
            Just shape_ -> { model | shape = shape_ }
            Nothing -> model
      Retract x_int
        ->
          case retractShape x_int model.shape of
            Just shape_ -> { model | shape = shape_ }
            Nothing -> model
      Edit_Mapping x_int string
        ->
          case Notation.toOuterFromRawOuter (Notation_Printing.toNotationFromNotationPrintable model.notation) x_int of
            PossibleCase (result_result_result_outer)
              ->
                case result_result_result_outer of
                  Ok result_result_outer
                    ->
                      case result_result_outer of
                        Ok result_outer
                          ->
                            case result_outer of
                              Ok outer
                                ->
                                  let
                                    x_int_ = Notation.toRawOuterFromOuter outer
                                  in
                                    { model | mapping = updateMapping (Array.toList x_int_) string model.mapping }
                              Err e -> model
                        Err e -> model
                  Err e -> model
            ImpossibleCase -> model
      Edit_Memo x_int string
        ->
          case Notation.toOuterFromRawOuter (Notation_Printing.toNotationFromNotationPrintable model.notation) x_int of
            PossibleCase (result_result_result_outer)
              ->
                case result_result_result_outer of
                  Ok result_result_outer
                    ->
                      case result_result_outer of
                        Ok result_outer
                          ->
                            case result_outer of
                              Ok outer
                                ->
                                  let
                                    x_int_ = Notation.toRawOuterFromOuter outer
                                  in
                                    { model | memo = updateMemo (Array.toList x_int_) string model.memo }
                              Err e -> model
                        Err e -> model
                  Err e -> model
            ImpossibleCase -> model

updateMapping : List Int -> String -> Mapping -> Mapping
updateMapping k v (Mapping dict) = Mapping (Dict.insert k v dict)

updateMemo : List Int -> String -> Memo -> Memo
updateMemo k v (Memo dict) = Memo (Dict.insert k v dict)

view : Model term -> Html Message
view model =
  div
    []
    [
      view_helper_1 model model.shape Array.empty
    ]

view_helper_1 : Model term -> Shape -> Array Int -> Html Message
view_helper_1 model (Shape shape) x_int
  =
    div
      []
      [
        div
          [
            css
              [
                displayFlex
              ,
                alignItems center
              ,
                padding2 (px 0) (px 8)
              ]
          ]
          [
            div
              [
                css
                  [
                    padding2 (px 16) (px 8)
                  ]
              ]
              [
                button
                  [
                    onClick (Expand (Array.push (Array.length shape) x_int))
                  ,
                    css
                      [
                        padding (px 8)
                      ,
                        borderStyle none
                      ,
                        backgroundColor (rgb 173 216 230)
                      ,
                        boxShadow4 (px 2) (px 2) (px 2) (rgb 208 208 208)
                      ,
                        fontSize (px 24)
                      ,
                        color (rgb 255 255 255)
                      ,
                        hover
                          [
                            backgroundColor (rgb 208 208 208)
                          ,
                            boxShadow none
                          ]
                      ]
                  ]
                  [
                    text "Expand"
                  ]
              ]
          ,
            div
              [
                css
                  [
                    padding2 (px 16) (px 8)
                  ]
              ]
              [
                button
                  [
                    onClick (Retract x_int)
                  ,
                    css
                      [
                        padding (px 8)
                      ,
                        borderStyle none
                      ,
                        backgroundColor (rgb 173 216 230)
                      ,
                        boxShadow4 (px 2) (px 2) (px 2) (rgb 208 208 208)
                      ,
                        fontSize (px 24)
                      ,
                        color (rgb 255 255 255)
                      ,
                        hover
                          [
                            backgroundColor (rgb 208 208 208)
                          ,
                            boxShadow none
                          ]
                      ]
                  ]
                  [
                    text "Retract"
                  ]
              ]
          ,
            div
              [
                css
                  [
                    padding2 (px 16) (px 8)
                  ]
              ]
              [
                case Notation.toTermFromRawOuter (Notation_Printing.toNotationFromNotationPrintable model.notation) x_int of
                  PossibleCase result_result_term
                    ->
                      case result_result_term of
                        Ok result_term
                          ->
                            case result_term of
                              Ok term
                                ->
                                  span
                                    [
                                      css
                                        [
                                          padding (px 8)
                                        ,
                                          fontSize (px 24)
                                        ]
                                    ]
                                    [
                                      text (model.notation.print term)
                                    ]
                              Err e
                                ->
                                  span
                                    [
                                      css
                                        [
                                          padding (px 8)
                                        ,
                                          fontSize (px 24)
                                        ,
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
                                    padding (px 8)
                                  ,
                                    fontSize (px 24)
                                  ,
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
                              padding (px 8)
                            ,
                              fontSize (px 24)
                            ,
                              color (rgb 220 20 60)
                            ]
                        ]
                        [
                          text "Fatal Error: An impossible case happened. Please report this error."
                        ]
              ]
          ]
      ,
        div
          [
            css
              [
                padding2 (px 0) (px 36)
              ]
          ]
          [
            textarea
              [
                onInput (Edit_Mapping x_int)
              ,
                css
                  [
                    height (px 80)
                  ,
                    width (px 400)
                  ,
                    borderStyle none
                  ,
                    backgroundColor (rgb 220 220 220)
                  ]
              ]
              [
                case Notation.toOuterFromRawOuter (Notation_Printing.toNotationFromNotationPrintable model.notation) x_int of
                  PossibleCase (result_result_result_outer)
                    ->
                      case result_result_result_outer of
                        Ok result_result_outer
                          ->
                            case result_result_outer of
                              Ok result_outer
                                ->
                                  case result_outer of
                                    Ok outer
                                      ->
                                        let
                                          x_int_ = Notation.toRawOuterFromOuter outer
                                        in
                                          case viewMapping (Array.toList x_int_) model.mapping of
                                            Just string -> text string
                                            Nothing -> text ""
                                    Err e -> text ""
                              Err e -> text ""
                        Err e -> text ""
                  ImpossibleCase -> text ""
              ]
          ]
      ,
        div
          [
            css
              [
                padding2 (px 0) (px 36)
              ]
          ]
          [
            textarea
              [
                onInput (Edit_Memo x_int)
              ,
                css
                  [
                    height (px 80)
                  ,
                    width (px 400)
                  ,
                    borderStyle none
                  ,
                    backgroundColor (rgb 220 220 220)
                  ]
              ]
              [
                case Notation.toOuterFromRawOuter (Notation_Printing.toNotationFromNotationPrintable model.notation) x_int of
                  PossibleCase (result_result_result_outer)
                    ->
                      case result_result_result_outer of
                        Ok result_result_outer
                          ->
                            case result_result_outer of
                              Ok result_outer
                                ->
                                  case result_outer of
                                    Ok outer
                                      ->
                                        let
                                          x_int_ = Notation.toRawOuterFromOuter outer
                                        in
                                          case viewMemo (Array.toList x_int_) model.memo of
                                            Just string -> text string
                                            Nothing -> text ""
                                    Err e -> text ""
                              Err e -> text ""
                        Err e -> text ""
                  ImpossibleCase -> text ""
              ]
          ]
      ,
        div
          [
            css
              [
                padding2 (px 0) (px 18)
              ]
          ]
          (Array.toList (Array.indexedMap (\int shape_ -> view_helper_1 model shape_ (Array.push int x_int)) shape))
      ]

viewMapping : List Int -> Mapping -> Maybe String
viewMapping k (Mapping dict) = Dict.get k dict

viewMemo : List Int -> Memo -> Maybe String
viewMemo k (Memo dict) = Dict.get k dict
