module Main
  exposing
    (
      Shape (..)
    ,
      expandShape
    ,
      retractShape
    ,
      Mapping (..)
    ,
      emptyMapping
    ,
      getMapping
    ,
      insertMapping
    ,
      emptyMemo
    ,
      getMemo
    ,
      insertMemo
    ,
      Message (..)
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

import Html.Styled exposing (Html, toUnstyled, div, button, textarea, text)
import Html.Styled.Events exposing (onClick)

import Browser

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

type Message = Expand (Array Int) | Retract (Array Int) | Edit_Mapping (Array Int) String | Edit_Memo (Array Int) String

-- 基本的にモデルの操作は単純に。
-- モデルに不整合が出る操作は view で弾く。
-- Model でチェックして、 view でもチェックする必要が
-- あるのなら、 View だけでチェックしたほうが簡単である。
-- ボタンの色を薄くするなどの処理が必要なので、
-- view でもチェックする必要があるんだよねえ……
type alias Model
  =
    {
      shape : Shape
    ,
      mapping : Mapping
    ,
      memo : Memo
    }

initialize : Model
initialize
  =
    {
      shape = Shape Array.empty
    ,
      mapping = emptyMapping
    ,
      memo = emptyMemo
    }

update : Message -> Model -> Model
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
      Edit_Mapping _ _ -> model
      Edit_Memo _ _ -> model

view : Model -> Html Message
view model =
  div
    []
    [
      view_helper_1 model model.shape Array.empty
    ]

view_helper_1 : Model -> Shape -> Array Int -> Html Message
view_helper_1 model (Shape shape) x_int
  =
    div
      []
      [
        div
          []
          [
            button
              [
                onClick (Expand (Array.push (Array.length shape) x_int))
              ]
              [
                text "Expand"
              ]
          ,
            button
              [
                onClick (Retract x_int)
              ]
              [
                text "Retract"
              ]
          ]
      ,
        div
          []
          [
            textarea
              []
              []
          ]
      ,
        div
          []
          (Array.toList (Array.indexedMap (\int shape_ -> view_helper_1 model shape_ (Array.push int x_int)) shape))
      ]

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
