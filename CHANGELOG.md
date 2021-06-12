# changelog

## not yet released

https://gitlab.com/Hexirp/analysis-tree/-/merge_requests/16 により次の変更が行われた。

* 共終タイプが 1 である行列に対応するノードを Expand で展開するとエラーのノードが出てくるバグを修正した。
  * このバグは、具体的に言えば `(0)(1)(0)` を展開しようとすると `(0)(1)` にならずにエラーになるというものである。

https://gitlab.com/Hexirp/analysis-tree/-/merge_requests/15 により次の変更が行われた。

* 木構造におけるノードが自らに対応する項を表示するようになった。

https://gitlab.com/Hexirp/analysis-tree/-/merge_requests/14 により次の変更が行われた。

* "Expand" ボタンと "Retact" ボタンを使って木構造を展開したり折り畳みしたりできるようになった。
* UI がモダンなデザインとなった。

https://gitlab.com/Hexirp/analysis-tree/-/merge_requests/13 により次の変更が行われた。

* リンターを導入した。

https://gitlab.com/Hexirp/analysis-tree/-/merge_requests/12 により次の変更が行われた。

* `Notation` モジュールという、複数の表記を横断して操作を抽象化するモジュールを追加した。
* `Notation` モジュールの導入にコードが対応できていないので、 UI を一旦削除した。

## 0.4.0

https://gitlab.com/Hexirp/analysis-tree/-/merge_requests/8 により次の変更が行われた。

* 木構造を表示して操作する機能を実装した。
* それぞれのノードに解析の記録とメモを書き込む機能を実装した。

https://gitlab.com/Hexirp/analysis-tree/-/merge_requests/7 により次の変更が行われた。

* `fromAlphaToBeta` を `toBetaFromAlpha` へ改名する。たとえば、 `fromSyntaxTreeToAst` を `toAstFromSyntaxTree` へ改名する。
  * `calcBetaFromAlpha` と語順が同じになるため。
  * 関数を適用する時に、 `toGammaFromBeta (toBetaFromAlpha alpha)` という風に方向が合うため。
* `BMS_4.Parsing` モジュールの内容を、基本的な型が上に来るように並べ替えた。
  * モジュールの内容を分かりやすくするため。

## 0.3.0

https://gitlab.com/Hexirp/analysis-tree/-/merge_requests/6 により次の変更が行われた。

* `expandMatrix` などの、行列の展開に関連する定義を追加する。

https://gitlab.com/Hexirp/analysis-tree/-/merge_requests/4 により次の変更が行われた。

* `fromAlphaToBeta` を `toBetaFromAlpha` へ改名する。たとえば、 `fromListToRawMatrix` を `toRawMatrixFromList` へ改名する。

## 0.2.0

https://gitlab.com/Hexirp/analysis-tree/-/merge_requests/1 により次の変更が行われた。

* `Case` モジュールを追加する。
* `BMS_4` モジュールに `RawMatrix` 型と `RawPatrix` 型を追加する。
* `List` 型と `Array` 型ではなく `RawMatrix` 型と `RawPatrix` 型を中心にして関数を定義する。
* `Matrix` と `Patrix` の相互変換を実装する。

## 0.1.0

散発的なコミットにより次の変更が行われた。

* `Array.Extra.Folding` モジュールを追加する。
* `BMS_4.Parsing` モジュールを追加する。
* `BMS_4` モジュールを追加する。
* `Main` モジュールを追加する。

## 0.0.0

何もない状態である。
