# 折り返しのルール

どのような場合に折り返すかのルールです。これは、どのような言語においても汎用的に適用でき、単純な規則の上に成り立っているという点で、最も美しいルールだと考えています。少なくとも、私はこのルールが好きです。

## モデル

Elm のプログラムは、多分木であるというように考えます。

それぞれのノードは、以下のようなルールを持ちます。

1. 自身の記述に、 1 行で 80 文字を超える部分があったら、自身を折り返す。
2. 子の要素が折り返されていたら、自身も折り返す。

一つの文法には、複数の書き方があります。それらには優先順位があります。折り返しをしない書き方が複数あったり、折り返しをしない書き方が存在しなかったりするので、その書き方が折り返しであるかどうかを明記します。

## インポートの宣言

`import Alpha_Module as A_Module exposing (A_Definition, B_Definition)` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
import Alpha_Module as A_Module exposing (A_Definition, B_Definition)
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
import Alpha_Module as A_Module
  exposing (A_Definition, B_Definition)
```
</td>
</tr>
<tr>
<td>
3
</td>
<td>
あり
</td>
<td>
```
import Alpha_Module
  as A_Module
  exposing (A_Definition, B_Definition)
```
</td>
</tr>
<tr>
<td>
4
</td>
<td>
あり
</td>
<td>
```
import
  Alpha_Module
  as A_Module
  exposing (A_Definition, B_Definition)
```
</td>
</tr>
</table>

### インポートの宣言の as

`as A_Module` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
as A_Module
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
as
  A_Module
```
</td>
</tr>
</table>

## インポートの宣言の exposing

`exposing (A_Definition, B_Definition)` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
exposing (A_Definition, B_Definition)
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
exposing
  (A_Definition, B_Definition)
```
</td>
</tr>
</table>

## 型の宣言

`alpha_value : Alpha_Type` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
alpha_value : Alpha_Type
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
alpha_value
  : Alpha_Type
```
</td>
</tr>
<tr>
<td>
3
</td>
<td>
あり
</td>
<td>
```
alpha_value
  :
    Alpha_Type
```
</td>
</tr>
</table>

## 定義の宣言

`alpha_value = alpha_definition` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
alpha_value = alpha_definition
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
alpha_value
  = alpha_definition
```
</td>
</tr>
<tr>
<td>
3
</td>
<td>
あり
</td>
<td>
```
alpha_value
  =
    alpha_definition
```
</td>
</tr>
</table>

## 関数の適用の式

`function alpha beta gamma` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
function alpha beta gamma
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
function
  alpha
  beta
  gamma
```
</td>
</tr>
</table>

## ラムダ式

`\\x -> expression` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
\\x -> expression
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
\\x
  -> expression
```
</td>
</tr>
<tr>
<td>
3
</td>
<td>
あり
</td>
<td>
```
\\x
  ->
    expression
```
</td>
</tr>
</table>

## case 式

`case x of\\n  alpha_pattern -> alpha_expression\\n  beta_pattern -> beta_expression` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
あり
</td>
<td>
```
case x of
  alpha_pattern -> alpha_expression
  beta_pattern -> beta_expression
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
case
  x
of
  alpha_pattern -> alpha_expression
  beta_pattern -> beta_expression
```
</td>
</tr>
</table>

### case 式の枝

`x -> expression` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
x -> expression
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
x
  -> expression
```
</td>
</tr>
<tr>
<td>
3
</td>
<td>
あり
</td>
<td>
```
x
  ->
    expression
```
</td>
</tr>
</table>

## if 式

`if x then alpha_expression else beta_expression` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
if x then alpha_expression else beta_expression
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
if x
  then alpha_expression
  else beta_expression
```
</td>
</tr>
<tr>
<td>
3
</td>
<td>
あり
</td>
<td>
```
if
  x
  then alpha_expression
  else beta_expression
```
</td>
</tr>
</table>

### if 式の then

`then alpha_expression` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
then alpha_expression
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
then
  alpha_expression
```
</td>
</tr>
</table>

### if 式の else

`else beta_expression` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
else beta_expression
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
else
  beta_expression
```
</td>
</tr>
</table>

## let 式

`let\\n  alpha_declaration\\n  beta_declaration\\nin\\n  expression` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
あり
</td>
<td>
```
let
  alpha_declaration
  beta_declaration
in
  expression
```
</td>
</tr>
</table>

### 二項演算子の式

`alpha + beta` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
alpha + beta
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
alpha
  + beta
```
</td>
</tr>
<tr>
<td>
3
</td>
<td>
あり
</td>
<td>
```
alpha
  +
    beta
```
</td>
</tr>
</table>

## 角括弧による列挙

`[x, y, z]` および `[ x, y, z ]` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
[x, y, z]
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
[
  x,
  y,
  z
]
```
</td>
</tr>
</table>

## 丸括弧による列挙

`(x, y, z)` です。

<table>
<tr>
<th>
優先順位
</th>
<th>
折り返し
</th>
<th>
テキスト
</th>
</tr>
<tr>
<td>
1
</td>
<td>
なし
</td>
<td>
```
(x, y, z)
```
</td>
</tr>
<tr>
<td>
2
</td>
<td>
あり
</td>
<td>
```
(
  x,
  y,
  z
)
```
</td>
</tr>
</table>
