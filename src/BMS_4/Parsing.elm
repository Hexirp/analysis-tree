module BMS_4.Parsing
  exposing
    (
      SyntaxTree,
      Expression (..),
      Matrix (..),
      Matrix0 (..),
      Matrix00 (..),
      Matrix01 (..),
      Row (..),
      Row0 (..),
      Row00 (..),
      Row01 (..),
      NaturalNumber (..),
      Digit (..),
      NonZeroDigit (..),
      SpacesAndBreaks (..),
      Spaces (..),
      SpaceAndBreak (..),
      Break (..),
      Space (..),
      Symbol_09 (..),
      Symbol_0A (..),
      Symbol_0D (..),
      Symbol_0D0A (..),
      Symbol_20 (..),
      Symbol_28 (..),
      Symbol_29 (..),
      Symbol_2C (..),
      Symbol_30 (..),
      Symbol_31 (..),
      Symbol_32 (..),
      Symbol_33 (..),
      Symbol_34 (..),
      Symbol_35 (..),
      Symbol_36 (..),
      Symbol_37 (..),
      Symbol_38 (..),
      Symbol_39 (..),
      Ast,
      Ast_Matrix,
      Ast_Sequence,
      Ast_NaturalNumber,
      fromSyntaxTreeToAst,
      fromExpression,
      fromMatrix,
      fromMatrix0,
      fromMatrix00,
      fromMatrix01,
      fromRow,
      fromRow0,
      fromRow00,
      fromRow01,
      fromNaturalNumber,
      fromDigit,
      fromNonZeroDigit,
      fromSpacesAndBreaks,
      fromSpaces,
      fromSpaceAndBreak,
      fromBreak,
      fromSpace,
      parse,
      parseExpression,
      parseMatrix,
      parseMatrix0,
      parseMatrix00,
      parseMatrix01,
      parseRow,
      parseRow0,
      parseRow00,
      parseRow01,
      parseNaturalNumber,
      parseDigit,
      parseNonZeroDigit,
      parseSpacesAndBreaks,
      parseSpaces,
      parseSpaceAndBreak,
      parseBreak,
      parseSpace,
      parseSymbol_09,
      parseSymbol_0A,
      parseSymbol_0D,
      parseSymbol_0D0A,
      parseSymbol_20,
      parseSymbol_28,
      parseSymbol_29,
      parseSymbol_2C,
      parseSymbol_30,
      parseSymbol_31,
      parseSymbol_32,
      parseSymbol_33,
      parseSymbol_34,
      parseSymbol_35,
      parseSymbol_36,
      parseSymbol_37,
      parseSymbol_38,
      parseSymbol_39,
      fromStringToAst,
      fromAstToString,
      brackets,
      braces
    )

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
-}

import Parser
  exposing (Parser, succeed, (|=), (|.), oneOf, backtrackable, lazy, symbol)

-- 構文木

type alias SyntaxTree = Expression

type Expression = Expression SpacesAndBreaks Matrix

type Matrix = Matrix (Maybe Matrix0)

type Matrix0 = Matrix0 Row (List Matrix00) (Maybe Matrix01)

type Matrix00 = Matrix00 SpacesAndBreaks Row

type Matrix01 = Matrix01 SpacesAndBreaks

type Row = Row Symbol_28 Spaces (Maybe Row0) Symbol_29

type Row0 = Row0 NaturalNumber Spaces (List Row00) (Maybe Row01)

type Row00 = Row00 Symbol_2C Spaces NaturalNumber Spaces

type Row01 = Row01 Symbol_2C Spaces

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

-- 抽象構文木

type alias Ast = Ast_Matrix

type alias Ast_Matrix = List Ast_Sequence

type alias Ast_Sequence = List Ast_NaturalNumber

type alias Ast_NaturalNumber = Int

-- 構文木から抽象構文木への変換

fromSyntaxTreeToAst : SyntaxTree -> Ast
fromSyntaxTreeToAst = fromExpression

fromExpression : Expression -> Ast_Matrix
fromExpression expression
  =
    case expression of
      Expression _ matrix -> fromMatrix matrix

fromMatrix : Matrix -> Ast_Matrix
fromMatrix matrix
  =
    case matrix of
      Matrix maybe_matrix0
        ->
          case maybe_matrix0 of
            Nothing -> []
            Just matrix0 -> fromMatrix0 matrix0

fromMatrix0 : Matrix0 -> Ast_Matrix
fromMatrix0 matrix0
  =
    case matrix0 of
      Matrix0 row list_matrix00 _
        -> fromRow row :: List.map fromMatrix00 list_matrix00

fromMatrix00 : Matrix00 -> Ast_Sequence
fromMatrix00 matrix00
  =
    case matrix00 of
      Matrix00 _ row -> fromRow row

fromMatrix01 : Matrix01 -> ()
fromMatrix01 _ = ()

fromRow : Row -> Ast_Sequence
fromRow row
  =
    case row of
      Row _ _ maybe_row0 _
        ->
          case maybe_row0 of
            Nothing -> []
            Just row0 -> fromRow0 row0

fromRow0 : Row0 -> Ast_Sequence
fromRow0 row0
  =
    case row0 of
      Row0 naturalNumber _ list_row00 _
        ->
          fromNaturalNumber naturalNumber
            :: List.map fromRow00 list_row00

fromRow00 : Row00 -> Ast_NaturalNumber
fromRow00 row00
  =
    case row00 of
      Row00 _ _ naturalNumber _
        -> fromNaturalNumber naturalNumber

fromRow01 : Row01 -> ()
fromRow01 _ = ()

fromNaturalNumber : NaturalNumber -> Ast_NaturalNumber
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

-- パーサー

parse : Parser SyntaxTree
parse = parseExpression |. Parser.end

parseExpression : Parser Expression
parseExpression = succeed Expression |= parseSpacesAndBreaks |= parseMatrix

parseMatrix : Parser Matrix
parseMatrix = succeed Matrix |= brackets parseMatrix0

parseMatrix0 : Parser Matrix0
parseMatrix0
  =
    succeed Matrix0
      |= parseRow
      |= braces parseMatrix00
      |= brackets parseMatrix01

parseMatrix00 : Parser Matrix00
parseMatrix00 = succeed Matrix00 |= parseSpacesAndBreaks |= parseRow

parseMatrix01 : Parser Matrix01
parseMatrix01 = succeed Matrix01 |= parseSpacesAndBreaks

parseRow : Parser Row
parseRow
  =
    succeed Row
      |= parseSymbol_28
      |= parseSpaces
      |= brackets parseRow0
      |= parseSymbol_29

parseRow0 : Parser Row0
parseRow0
  =
    succeed Row0
      |= parseNaturalNumber
      |= parseSpaces
      |= braces parseRow00
      |= brackets parseRow01

parseRow00 : Parser Row00
parseRow00
  =
    succeed Row00
      |= parseSymbol_2C
      |= parseSpaces
      |= parseNaturalNumber
      |= parseSpaces

parseRow01 : Parser Row01
parseRow01 = succeed Row01 |= parseSymbol_2C |= parseSpaces

parseNaturalNumber : Parser NaturalNumber
parseNaturalNumber
  =
    oneOf
      [
        backtrackable (succeed NaturalNumber_0 |= parseSymbol_30),
        succeed NaturalNumber_1 |= parseNonZeroDigit |= braces parseDigit
      ]

parseDigit : Parser Digit
parseDigit
  =
    oneOf
      [
        backtrackable (succeed Digit_0 |= parseSymbol_30),
        backtrackable (succeed Digit_1 |= parseSymbol_31),
        backtrackable (succeed Digit_2 |= parseSymbol_32),
        backtrackable (succeed Digit_3 |= parseSymbol_33),
        backtrackable (succeed Digit_4 |= parseSymbol_34),
        backtrackable (succeed Digit_5 |= parseSymbol_35),
        backtrackable (succeed Digit_6 |= parseSymbol_36),
        backtrackable (succeed Digit_7 |= parseSymbol_37),
        backtrackable (succeed Digit_8 |= parseSymbol_38),
        succeed Digit_9 |= parseSymbol_39
      ]

parseNonZeroDigit : Parser NonZeroDigit
parseNonZeroDigit
  =
    oneOf
      [
        backtrackable (succeed NonZeroDigit_0 |= parseSymbol_31),
        backtrackable (succeed NonZeroDigit_1 |= parseSymbol_32),
        backtrackable (succeed NonZeroDigit_2 |= parseSymbol_33),
        backtrackable (succeed NonZeroDigit_3 |= parseSymbol_34),
        backtrackable (succeed NonZeroDigit_4 |= parseSymbol_35),
        backtrackable (succeed NonZeroDigit_5 |= parseSymbol_36),
        backtrackable (succeed NonZeroDigit_6 |= parseSymbol_37),
        backtrackable (succeed NonZeroDigit_7 |= parseSymbol_38),
        succeed NonZeroDigit_8 |= parseSymbol_39
      ]

parseSpacesAndBreaks : Parser SpacesAndBreaks
parseSpacesAndBreaks = succeed SpacesAndBreaks |= braces parseSpaceAndBreak

parseSpaces : Parser Spaces
parseSpaces = succeed Spaces |= braces parseSpace

parseSpaceAndBreak : Parser SpaceAndBreak
parseSpaceAndBreak
  =
    oneOf
      [
        backtrackable (succeed SpaceAndBreak_0 |= parseSpace),
        succeed SpaceAndBreak_1 |= parseBreak
      ]

parseBreak : Parser Break
parseBreak
  =
    oneOf
      [
        backtrackable (succeed Break_1 |= parseSymbol_0D0A),
        backtrackable (succeed Break_0 |= parseSymbol_0A),
        succeed Break_2 |= parseSymbol_0D
      ]

parseSpace : Parser Space
parseSpace
  =
    oneOf
      [
        backtrackable (succeed Space_0 |= parseSymbol_20),
        succeed Space_1 |= parseSymbol_09
      ]

parseSymbol_09 : Parser Symbol_09
parseSymbol_09 = succeed Symbol_09 |. symbol "\t"

parseSymbol_0A : Parser Symbol_0A
parseSymbol_0A = succeed Symbol_0A |. symbol "\n"

parseSymbol_0D : Parser Symbol_0D
parseSymbol_0D = succeed Symbol_0D |. symbol "\r"

parseSymbol_0D0A : Parser Symbol_0D0A
parseSymbol_0D0A = succeed Symbol_0D0A |. symbol "\r\n"

parseSymbol_20 : Parser Symbol_20
parseSymbol_20 = succeed Symbol_20 |. symbol " "

parseSymbol_28 : Parser Symbol_28
parseSymbol_28 = succeed Symbol_28 |. symbol "("

parseSymbol_29 : Parser Symbol_29
parseSymbol_29 = succeed Symbol_29 |. symbol ")"

parseSymbol_2C : Parser Symbol_2C
parseSymbol_2C = succeed Symbol_2C |. symbol ","

parseSymbol_30 : Parser Symbol_30
parseSymbol_30 = succeed Symbol_30 |. symbol "0"

parseSymbol_31 : Parser Symbol_31
parseSymbol_31 = succeed Symbol_31 |. symbol "1"

parseSymbol_32 : Parser Symbol_32
parseSymbol_32 = succeed Symbol_32 |. symbol "2"

parseSymbol_33 : Parser Symbol_33
parseSymbol_33 = succeed Symbol_33 |. symbol "3"

parseSymbol_34 : Parser Symbol_34
parseSymbol_34 = succeed Symbol_34 |. symbol "4"

parseSymbol_35 : Parser Symbol_35
parseSymbol_35 = succeed Symbol_35 |. symbol "5"

parseSymbol_36 : Parser Symbol_36
parseSymbol_36 = succeed Symbol_36 |. symbol "6"

parseSymbol_37 : Parser Symbol_37
parseSymbol_37 = succeed Symbol_37 |. symbol "7"

parseSymbol_38 : Parser Symbol_38
parseSymbol_38 = succeed Symbol_38 |. symbol "8"

parseSymbol_39 : Parser Symbol_39
parseSymbol_39 = succeed Symbol_39 |. symbol "9"

-- 文字列から抽象構文木への変換

fromStringToAst : String -> Maybe Ast
fromStringToAst string
  =
    case Parser.run parse string of
      Ok syntaxTree -> Just (fromSyntaxTreeToAst syntaxTree)
      Err _ -> Nothing

-- 抽象構文木から文字列への変換

fromAstToString : Ast -> String
fromAstToString ast
  =
    List.foldl (++) "" (List.map fromAstToString_helper ast)

fromAstToString_helper : List Int -> String
fromAstToString_helper list_int
  =
    case list_int of
      [] -> "()"
      list_int_p :: list_int_s
        ->
          "(" ++ String.fromInt list_int_p
            ++ List.foldl (\x r -> String.fromInt x ++ "," ++ r) ")" list_int_s

-- 汎用関数

brackets : Parser a -> Parser (Maybe a)
brackets x = oneOf [backtrackable (succeed Just |= x), succeed Nothing]

braces : Parser a -> Parser (List a)
braces x
  =
    oneOf
      [backtrackable (succeed (::) |= x |= lazy (\_ -> braces x)), succeed []]
