module Case exposing (Case (..), isValid, traverseArray)

{-| 型の上では値が存在しない可能性があるが、実際には値が存在されると期待される型です。

ある関数が `Case` 型の値を返す場合は、その関数は必ず `PossibleCase` に包まれた結果を返さなければなりません。 `ImpossibleCase` を返してはなりません。

# 型

@docs Case

# 関数たち

@docs isValid, traverseArray
-}

import Array exposing (Array)

{-| 値が存在することが期待されるが、型の上では値が存在しない可能性を排除できない値です。

たとえば、次のような場合に使います。

  case Array.get 1 [0,1,2] of
    Nothing -> ImpossibleCase
    Just int -> PossibleCase int
-}
type Case a = ImpossibleCase | PossibleCase a

{-| 或る `Case` 型の値が妥当であることを確かめます。 -}
isValid : Case a -> Bool
isValid case_x
  =
    case case_x of
      ImpossibleCase -> False
      PossibleCase _ -> True

{-| 或る配列を或る `Case` 型を返す関数によってトラバースします。 -}
traverseArray : (a -> Case b) -> Array a -> Case (Array b)
traverseArray f array_x
  =
    Array.foldl
      (\x case_r
        ->
          case f x of
            ImpossibleCase -> ImpossibleCase
            PossibleCase y
              ->
                case case_r of
                  ImpossibleCase -> ImpossibleCase
                  PossibleCase r -> PossibleCase (Array.push y r))
      (PossibleCase Array.empty)
      array_x
