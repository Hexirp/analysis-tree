module Parser_Extra exposing (brackets, braces)

{-| パーサーについての便利な関数たちです。

# 関数

@docs brackets

@docs braces
-}

import Parser exposing (..)

{-| 省略可能な部分を表すパーサーです。

EBNF における `[ ... ]` です。
-}
brackets : Parser a -> Parser (Maybe a)
brackets x = oneOf [backtrackable (succeed Just |= x), succeed Nothing]

{-| 省略可能かつ繰り返し可能な部分を表すパーサーです。

EBNF における `{ ... }` です。
-}
braces : Parser a -> Parser (List a)
braces x
  =
    oneOf
      [backtrackable (succeed (::) |= x |= lazy (\_ -> braces x)), succeed []]
