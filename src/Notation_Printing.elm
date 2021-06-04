module Notation_Printing
  exposing
    (
      NotationPrintable
    )

import String exposing (String)

import Notation exposing (Expander)

{-| 項を表示可能な基本列付きの順序数表記です。

`compare` は、或る二つの項を比較します。これは全順序でなければなりません。

`expand` は、或る項を或る自然数で展開します。其の自然数が其の基本列の長さよりも大きい時は `OutOfIndexError` となります。これは `x[n] < x` でなければなりません。これは `x[n] < x[n+1]` でなければなりません。

`maximum` は、表記の限界を表す項です。これが必要なのは、アプリケーションで利用する外表記が、表記の限界を表す項を必要とするからです。これは `a ≤ x → a = x` でなければなりません。

余談として、これは真の順序数表記であることを必要としませんが、もし `compare` が整礎であるならば真の順序数表記になります。
-}
type alias NotationPrintable term
  =
    {
      compare : term -> term -> Order
    ,
      expand : Expander term
    ,
      maximum : term
    ,
      print : term -> String
    }
