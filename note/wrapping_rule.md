# 折り返しのルール

どのような場合に折り返すかのルールです。これは、どのような言語においても汎用的に適用でき、単純な規則の上に成り立っているという点で、最も美しいルールだと考えています。少なくとも、私はこのルールが好きです。

## モデル

Elm のプログラムは、多分木であるというように考えます。

それぞれのノードは、以下のようなルールを持ちます。

1. 自身の記述に、 1 行で 80 文字を超える部分があったら、自身を折り返す。
2. 子の要素が折り返されていたら、自身も折り返す。
3. 1 と 2 を守っていれば、その範囲で自由に折り返しすることが出来る。

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
<pre><code>import Alpha_Module as A_Module exposing (A_Definition, B_Definition)</code></pre>
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
<pre><code>import Alpha_Module as A_Module
  exposing (A_Definition, B_Definition)</code></pre>
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
<pre><code>import Alpha_Module
  as A_Module
  exposing (A_Definition, B_Definition)</code></pre>
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
<pre><code>import
  Alpha_Module
  as A_Module
  exposing (A_Definition, B_Definition)</code></pre>
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
<pre><code>as A_Module</code></pre>
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
<pre><code>as
  A_Module</code></pre>
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
<pre><code>exposing (A_Definition, B_Definition)</code></pre>
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
<pre><code>exposing
  (A_Definition, B_Definition)</code></pre>
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
<pre><code>alpha_value : Alpha_Type</code></pre>
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
<pre><code>alpha_value
  : Alpha_Type</code></pre>
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
<pre><code>alpha_value
  :
    Alpha_Type</code></pre>
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
<pre><code>alpha_value = alpha_definition</code></pre>
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
<pre><code>alpha_value
  = alpha_definition</code></pre>
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
<pre><code>alpha_value
  =
    alpha_definition</code></pre>
</td>
</tr>
</table>

## 関数の適用の式

`function alpha` です。

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
<pre><code>function alpha</code></pre>
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
<pre><code>function
  alpha</code></pre>
</td>
</tr>
</table>

## ラムダ式

`\x -> expression` です。

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
<pre><code>\x -> expression</code></pre>
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
<pre><code>\x
  -> expression</code></pre>
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
<pre><code>\x
  ->
    expression</code></pre>
</td>
</tr>
</table>

## case 式

`case x of\n  alpha_pattern -> alpha_expression\n  beta_pattern -> beta_expression` です。

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
<pre><code>case x of
  alpha_pattern -> alpha_expression
  beta_pattern -> beta_expression</code></pre>
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
<pre><code>case
  x
of
  alpha_pattern -> alpha_expression
  beta_pattern -> beta_expression</code></pre>
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
<pre><code>x -> expression</code></pre>
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
<pre><code>x
  -> expression</code></pre>
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
<pre><code>x
  ->
    expression</code></pre>
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
<pre><code>if x then alpha_expression else beta_expression</code></pre>
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
<pre><code>if x
  then alpha_expression
  else beta_expression</code></pre>
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
<pre><code>if
  x
  then alpha_expression
  else beta_expression</code></pre>
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
<pre><code>then alpha_expression</code></pre>
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
<pre><code>then
  alpha_expression</code></pre>
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
<pre><code>else beta_expression</code></pre>
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
<pre><code>else
  beta_expression</code></pre>
</td>
</tr>
</table>

## let 式

`let\n  alpha_declaration\n  beta_declaration\nin\n  expression` です。

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
<pre><code>let
  alpha_declaration
  beta_declaration
in
  expression</code></pre>
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
<pre><code>alpha + beta</code></pre>
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
<pre><code>alpha
  + beta</code></pre>
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
<pre><code>alpha
  +
    beta</code></pre>
</td>
</tr>
</table>

## 角括弧による列挙

`[x,y,z]` および `[x, y, z]` および `[ x, y, z ]` です。

コンマを折り返すのは、中の式が巨大になったせいでコンマを入れ忘れがちな時に使ってください。

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
<pre><code>[x, y, z]</code></pre>
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
<pre><code>[
  x,
  y,
  z
]</code></pre>
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
<pre><code>[
  x
,
  y
,
  z
]</code></pre>
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
<pre><code>(x, y, z)</code></pre>
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
<pre><code>(
  x,
  y,
  z
)</code></pre>
</td>
</tr>
</table>
