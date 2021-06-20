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

{-| analysis-tree を実装します。

# 定義
@docs Notation, Model, initialize, update, view, main
-}

import Html.Styled exposing (Html, toUnstyled)

import Browser

import Notation

import BMS_4
import BMS_4_Printing

import Main.Message exposing (Message)
import Main.Notation

{-| analysis-tree で使用可能な表記です。
-}
type Notation = Notation_BMS_4

{-| Model です。
-}
type alias Model
  =
    {
      notation : Notation
    ,
      model_BMS_4 : Main.Notation.Model (Notation.Maxipointed BMS_4.Matrix)
    }

{-| init です。
-}
initialize : Model
initialize
  =
    {
      notation = Notation_BMS_4
    ,
      model_BMS_4 = Main.Notation.initialize BMS_4_Printing.notation
    }

{-| update です。
-}
update : Message -> Model -> Model
update message model
  =
    case model.notation of
      Notation_BMS_4 -> { model | model_BMS_4 = Main.Notation.update message model.model_BMS_4 }

{-| view です。
-}
view : Model -> Html Message
view model
  =
    case model.notation of
      Notation_BMS_4 -> Main.Notation.view model.model_BMS_4

{-| main です。
-}
main : Program () Model Message
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
