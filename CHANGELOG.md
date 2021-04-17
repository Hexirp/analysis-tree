# changelog

## not yet released

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
