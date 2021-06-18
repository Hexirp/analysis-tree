module Main.Shape
  exposing
    (
      Shape (..)
    ,
      expandShape
    ,
      retractShape
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

{-| アプリケーションの木構造である。

木構造の節には、それぞれ自然数のリストが割り当てられる。たとえば、次のような木構造を考えよう。

  x
  ├ ─ x
  │   └ ─ x
  └ ─ x
      ├ ─ x
      │   ├ ─ x
      │   └ ─ x
      └ ─ x

これらには、次のような配列が割り当てられる。

  []
  [0]
  [0, 0]
  [1]
  [1, 0]
  [1, 0, 0]
  [1, 0, 1]
  [1, 1]
-}
type Shape = Shape (Array Shape)

{-| シェイプを展開する。

次のような木構造があったとしよう。

  x
  ├ ─ x
  │   └ ─ x
  └ ─ x
      ├ ─ x
      │   ├ ─ x
      │   └ ─ x
      └ ─ x

次のように追加可能な位置が示される。

  x
  ├ ─ x
  │   ├ ─ x
  │   │   └ ─ o
  │   └ ─ o
  ├ ─ x
  │   ├ ─ x
  │   │   ├ ─ x
  │   │   │   └ ─ o
  │   │   ├ ─ x
  │   │   │   └ ─ o
  │   │   └ ─ o
  │   ├ ─ x
  │   │   └ ─ o
  │   └ ─ o
  └ ─ o

`o` に該当する位置を配列で与えると、そのように木構造が更新される。

更新の仕方は、その要素が存在するかどうかのフラグを `True` にセットするようなセッターに似ていると考えてほしい。 `x` に該当する位置を配列で与えると、更新などは起こらず、エラーにもならない。それ以外の位置を指定すると、エラーとなる。
-}
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

{-| シェイプを折り畳みする。

次のような木構造があったとしよう。

  x
  ├ ─ x
  │   └ ─ x
  └ ─ x
      ├ ─ x
      │   ├ ─ x
      │   └ ─ x
      └ ─ x

次のように削除可能な位置が示される。

  x
  ├ ─ x
  │   └ ─ o
  └ ─ x
      ├ ─ x
      │   ├ ─ o
      │   └ ─ o
      └ ─ o

`o` に該当する位置を配列で与えると、そのように木構造が更新される。

更新の仕方は、その要素が存在するかどうかのフラグを `False` にセットするようなセッターに似ていると考えてほしい。 既に要素が存在しない位置を配列で与えると、更新などは起こらず、エラーにもならない。 `x` に該当する位置を指定すると、エラーとなる。
-}
retractShape : Array Int -> Shape -> Maybe Shape
retractShape x_int shape
  =
    case Array.toList x_int of
      [] -> Nothing
      xp :: xs -> retractShape_helper_1 xp xs shape

retractShape_helper_1 : Int -> List Int -> Shape -> Maybe Shape
retractShape_helper_1 xp xs (Shape shape)
  =
    case xs of
      []
        ->
          if 0 <= xp
            then
              if Array.length shape - 1 <= xp
                then
                  if Array.length shape - 1 < xp
                    then Just (Shape shape)
                    else
                      case Array.get xp shape of
                        Just (Shape shape_)
                          ->
                            if Array.isEmpty shape_
                              then Just (Shape (Array.pop shape))
                              else Nothing
                        Nothing -> Nothing
                else Nothing
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
                          maybe_shape__ = retractShape_helper_1 xsp xss shape_
                        in
                          case maybe_shape__ of
                            Just shape__ -> Just (Shape (Array.set xp shape__ shape))
                            Nothing -> Nothing
                    Nothing -> Nothing
            else Nothing
