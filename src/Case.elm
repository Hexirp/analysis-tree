module Case
  exposing
    (
      Case (..)
    ,
      isValid
    ,
      traverseArray
    ,
      sequenceArray
    ,
      initializeArrayWithCaseWithState
    )

{-| 型の上では値が存在しない可能性があるが、実際には値が存在されると期待される型です。

ある関数が `Case` 型の値を返す場合は、その関数は必ず `PossibleCase` に包まれた結果を返さなければなりません。 `ImpossibleCase` を返してはなりません。

# 型

@docs Case

# 関数たち

@docs isValid, traverseArray, sequenceArray, initializeArrayWithCaseWithState
-}

import Array exposing (Array)

{-| 値が存在することが期待されるが、型の上では値が存在しない可能性を排除できない値です。

たとえば、次のような場合に使います。

  case Array.get 1 [0,1,2] of
    Nothing -> ImpossibleCase
    Just int -> PossibleCase int
-}
type Case a = PossibleCase a | ImpossibleCase

{-| 或る `Case` 型の値が妥当であることを確かめます。 -}
isValid : Case a -> Bool
isValid case_x
  =
    case case_x of
      ImpossibleCase -> False
      PossibleCase _ -> True

{-| 或る配列を或る `Case` 型を返す関数によってトラバースします。 -}
traverseArray : (a -> Case b) -> Array a -> Case (Array b)
traverseArray f int_x
  =
    let
      func x case_int_x_
        =
          case f x of
            PossibleCase y
              ->
                case case_int_x_ of
                  PossibleCase int_x_ -> PossibleCase (Array.push y int_x_)
                  ImpossibleCase -> ImpossibleCase
            ImpossibleCase -> ImpossibleCase
    in
      Array.foldl func (PossibleCase Array.empty) int_x

{-| 或る配列を其の恒等関数によってトラバースします。 -}
sequenceArray : Array (Case a) -> Case (Array a)
sequenceArray int_x
  =
    let
      func case_x case_int_x_
        =
          case case_x of
            PossibleCase x
              ->
                case case_int_x_ of
                  PossibleCase int_x_ -> PossibleCase (Array.push x int_x_)
                  ImpossibleCase -> ImpossibleCase
            ImpossibleCase -> ImpossibleCase
    in
      Array.foldl func (PossibleCase Array.empty) int_x

{-| 或る配列を或る関数により生成します。 `initializeArrayWithCaseWithState int func state` は、その長さが `int` でインデックスが `i` の要素を `f i` である配列を返します。 `Case` と状態の作用が加わっています。 -}
initializeArrayWithCaseWithState : Int -> (Int -> state -> Case (a, state)) -> state -> Case (Array a, state)
initializeArrayWithCaseWithState int f s
  =
    let
      func n s_
        =
          if int <= n
            then PossibleCase ([], s_)
            else
              case f n s_ of
                PossibleCase (xp, s__)
                  ->
                    case func (n + 1) s__ of
                      PossibleCase (xs, s___) -> PossibleCase (xp :: xs, s___)
                      ImpossibleCase -> ImpossibleCase
                ImpossibleCase -> ImpossibleCase
    in
      case func 0 s of
        PossibleCase (list, s_)
          ->
            PossibleCase (Array.fromList list, s_)
        ImpossibleCase -> ImpossibleCase
