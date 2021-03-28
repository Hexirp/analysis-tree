module BMS_4.Parsing exposing (..)

{-| `BMS_4` でのパーサー部分だけを切り出したモジュールです。

基本的な構文は次のようになります。

  expression = spaces and breaks , matrix;
  matrix = [ row , { spaces and breaks , row } , [ spaces and breaks ] ];
  row = "(" , spaces , [ natural number , spaces , { "," , spaces , natural number , spaces } , [ "," , spaces ] ] , ")";
  natural number = "0" | non-zero digit , { digit };
  non-zero digit = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
  digit ="0" |  "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
  spaces and breaks = { space and break };
  spaces = { space };
  space and break = space | break;
  break = "\n" , ["\r"] | "\r";
  space = " " | "\t";

## 構文木

@docs Expression, Matrix, Matrix0, Matrix00, Matrix01, Row, Row0, Row00, Row01, NaturalNumber, Digit, NonZeroDigit, SpacesAndBreaks, Spaces, SpaceAndBreak, Break, Space, Symbol_09, Symbol_0A, Symbol_0D, Symbol_0D0A, Symbol_20, Symbol_28, Symbol_29, Symbol_2C, Symbol_30, Symbol_31, Symbol_32, Symbol_33, Symbol_34, Symbol_35, Symbol_36, Symbol_37, Symbol_38, Symbol_39
-}

import Basics exposing (..)

import Maybe exposing (Maybe)

import List

type Expression = Expression SpacesAndBreaks Matrix

type Matrix = Matrix (Maybe Matrix0)

type Matrix0 = Matrix0 Row (List Matrix00) (Maybe Matrix01)

type Matrix00 = Matrix00 SpacesAndBreaks Row

type Matrix01 = Matrix01 SpacesAndBreaks

type Row = Row Symbol_28 Spaces (Maybe Row0) Symbol_29

type Row0 = Row0 NaturalNumber Spaces (List Row00) (Maybe Row01)

type Row00 = Row00 Symbol_2C Spaces NaturalNumber Spaces

type Row01 = Row01 Symbol_2C

type NaturalNumber
  = NaturalNumber_0 Symbol_30 | NaturalNumber_1 NonZeroDigit (List Digit)

type Digit
  =
    Digit_0 Symbol_30
      | Digit_1 Symbol_31
      | Digit_2 Symbol_32
      | Digit_3 Symbol_33
      | Digit_4 Symbol_34
      | Digit_5 Symbol_35
      | Digit_6 Symbol_36
      | Digit_7 Symbol_37
      | Digit_8 Symbol_38
      | Digit_9 Symbol_39

type NonZeroDigit
  =
    NonZeroDigit_0 Symbol_31
      | NonZeroDigit_1 Symbol_32
      | NonZeroDigit_2 Symbol_33
      | NonZeroDigit_3 Symbol_34
      | NonZeroDigit_4 Symbol_35
      | NonZeroDigit_5 Symbol_36
      | NonZeroDigit_6 Symbol_37
      | NonZeroDigit_7 Symbol_38
      | NonZeroDigit_8 Symbol_39

type SpacesAndBreaks = SpacesAndBreaks (List SpaceAndBreak)

type Spaces = Spaces (List Space)

type SpaceAndBreak = SpaceAndBreak_0 Space | SpaceAndBreak_1 Break

type Break = Break_0 Symbol_0A | Break_1 Symbol_0D0A | Break_2 Symbol_0D

type Space = Space_0 Symbol_20 | Space_1 Symbol_09

type Symbol_09 = Symbol_09

type Symbol_0A = Symbol_0A

type Symbol_0D = Symbol_0D

type Symbol_0D0A = Symbol_0D0A

type Symbol_20 = Symbol_20

type Symbol_28 = Symbol_28

type Symbol_29 = Symbol_29

type Symbol_2C = Symbol_2C

type Symbol_30 = Symbol_30

type Symbol_31 = Symbol_31

type Symbol_32 = Symbol_32

type Symbol_33 = Symbol_33

type Symbol_34 = Symbol_34

type Symbol_35 = Symbol_35

type Symbol_36 = Symbol_36

type Symbol_37 = Symbol_37

type Symbol_38 = Symbol_38

type Symbol_39 = Symbol_39

fromExpression : Expression -> List (List Int)
fromExpression expression
  =
    case expression of
      Expression _ matrix -> fromMatrix matrix

fromMatrix : Matrix -> List (List Int)
fromMatrix matrix
  =
    case matrix of
      Matrix maybe_matrix0
        ->
          case maybe_matrix0 of
            Nothing -> []
            Just matrix0 -> fromMatrix0 matrix0

fromMatrix0 : Matrix0 -> List (List Int)
fromMatrix0 matrix0
  =
    case matrix0 of
      Matrix0 row list_matrix00 _
        -> fromRow row :: List.map fromMatrix00 list_matrix00

fromMatrix00 : Matrix00 -> List Int
fromMatrix00 matrix00
  =
    case matrix00 of
      Matrix00 _ row -> fromRow row

fromMatrix01 : Matrix01 -> ()
fromMatrix01 _ = ()

fromRow : Row -> List Int
fromRow row
  =
    case row of
      Row _ _ maybe_row0 _
        ->
          case maybe_row0 of
            Nothing -> []
            Just row0 -> fromRow0 row0

fromRow0 : Row0 -> List Int
fromRow0 row0
  =
    case row0 of
      Row0 naturalNumber _ list_row00 _
        ->
          fromNaturalNumber naturalNumber
            :: List.map fromRow00 list_row00

fromRow00 : Row00 -> Int
fromRow00 row00
  =
    case row00 of
      Row00 _ _ naturalNumber _
        -> fromNaturalNumber naturalNumber

fromRow01 : Row01 -> ()
fromRow01 _ = ()

fromNaturalNumber : NaturalNumber -> Int
fromNaturalNumber naturalNumber
  =
    case naturalNumber of
      NaturalNumber_0 _ -> 0
      NaturalNumber_1 nonZeroDigit list_digit
        ->
          fromNaturalNumber_helper
            (fromNonZeroDigit nonZeroDigit)
            (List.map fromDigit list_digit)

fromNaturalNumber_helper : Int -> List Int -> Int
fromNaturalNumber_helper x xs = List.foldl (\m n -> 10 * n + m) x xs

fromDigit : Digit -> Int
fromDigit digit
  =
    case digit of
      Digit_0 _ -> 0
      Digit_1 _ -> 1
      Digit_2 _ -> 2
      Digit_3 _ -> 3
      Digit_4 _ -> 4
      Digit_5 _ -> 5
      Digit_6 _ -> 6
      Digit_7 _ -> 7
      Digit_8 _ -> 8
      Digit_9 _ -> 9

fromNonZeroDigit : NonZeroDigit -> Int
fromNonZeroDigit nonZeroDigit
  =
    case nonZeroDigit of
      NonZeroDigit_0 _ -> 1
      NonZeroDigit_1 _ -> 2
      NonZeroDigit_2 _ -> 3
      NonZeroDigit_3 _ -> 4
      NonZeroDigit_4 _ -> 5
      NonZeroDigit_5 _ -> 6
      NonZeroDigit_6 _ -> 7
      NonZeroDigit_7 _ -> 8
      NonZeroDigit_8 _ -> 9

fromSpacesAndBreaks : SpacesAndBreaks -> ()
fromSpacesAndBreaks _ = ()

fromSpaces : Spaces -> ()
fromSpaces _ = ()

fromSpaceAndBreak : SpaceAndBreak -> ()
fromSpaceAndBreak _ = ()

fromBreak : Break -> ()
fromBreak _ = ()

fromSpace : Space -> ()
fromSpace _ = ()
