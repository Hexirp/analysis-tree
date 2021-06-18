module Main
  exposing
    (
      Notation (..)
    ,
      Model
    ,
      initialize
    ,
      update
    ,
      view
    ,
      main
    )

import Dict exposing (Dict)

import Array exposing (Array)
import Array.Extra as Array

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

import Html.Styled exposing (Html, toUnstyled, div, button, textarea, span, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick, onInput)

import Browser

import Case exposing (Case (..))

import Notation
import Notation_Printing

import BMS_4
import BMS_4_Printing

import Main.Notation

type Notation = Notation_BMS_4

type alias Model
  =
    {
      notation : Notation
    ,
      model_BMS_4 : Main.Notation.Model (Notation.Maxipointed BMS_4.Matrix)
    }

initialize : Model
initialize
  =
    {
      notation = Notation_BMS_4
    ,
      model_BMS_4 = Main.Notation.initialize BMS_4_Printing.notation
    }

update : Main.Notation.Message -> Model -> Model
update message model
  =
    case model.notation of
      Notation_BMS_4 -> { model | model_BMS_4 = Main.Notation.update message model.model_BMS_4 }

view : Model -> Html Main.Notation.Message
view model
  =
    case model.notation of
      Notation_BMS_4 -> Main.Notation.view model.model_BMS_4

main : Program () Model Main.Notation.Message
main
  =
    Browser.sandbox
      {
        init = initialize
      ,
        view = \model -> toUnstyled (view model)
      ,
        update = update
      }
