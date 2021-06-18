module Main.Message
  exposing
    (
      Message (..)
    )

import Array exposing (Array)

type Message
  =
    Expand (Array Int)
  |
    Retract (Array Int)
  |
    Edit_Mapping (Array Int) String
  |
    Edit_Memo (Array Int) String
