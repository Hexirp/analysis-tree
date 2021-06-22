module Array_Extra
  exposing
    (
      all
    ,
      maximum
    ,
      minimum
    )

{-| 配列についての便利な折り畳み系の関数たちです。

# 関数

@docs all, maximum, minimum
-}

import Array exposing (..)

{-| いくつかの検査を全ての要素が満たすか決定します。 -}
all : (a -> Bool) -> Array a -> Bool
all f array = foldl (\e r -> f e && r) True array

{-| 非空の配列から最大元を見つけます。 -}
maximum : Array comparable -> Maybe comparable
maximum array = Maybe.map (\e -> foldl max e array) (get 0 array)

{-| 非空の配列から最小元を見つけます。 -}
minimum : Array comparable -> Maybe comparable
minimum array = Maybe.map (\e -> foldl min e array) (get 0 array)
