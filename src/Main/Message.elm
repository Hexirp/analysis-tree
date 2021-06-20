module Main.Message
  exposing
    (
      Message (..)
    )

{-| `Message` 型を定義します。これは analysis-tree における Msg そのものです。

# Msg
@docs Message
-}

import Array exposing (Array)

{-| Msg です。
-}
type Message
  =
    Expand (Array Int)
  |
    Retract (Array Int)
  |
    Edit_Mapping (Array Int) String
  |
    Edit_Memo (Array Int) String
