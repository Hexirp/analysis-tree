# Analysis Tree

Analysis Tree は、巨大数論での表記の解析手法において、対象の表記と他の表記を対応づけ、そのために木構造を利用するようなものをサポートするツールである。このようなスタイルは、『[バシク行列システム４の解析](https://googology.wikia.org/ja/wiki/%E3%83%A6%E3%83%BC%E3%82%B6%E3%83%BC%E3%83%96%E3%83%AD%E3%82%B0:Hexirp/(2020-10-07)_%E3%83%90%E3%82%B7%E3%82%AF%E8%A1%8C%E5%88%97%E3%82%B7%E3%82%B9%E3%83%86%E3%83%A0_4_%E3%81%AE%E8%A7%A3%E6%9E%90)』で示したものであり、しかし木構造を記録しておくことが大変であるため断念したものである。 [GitLab Pages によりホストされたページ](https://hexirp.gitlab.io/analysis-tree/)で利用できる。

Naruyoko さんが、数日前に[同じようなツール](https://github.com/Naruyoko/yaBMS/blob/41f9fde0e53e455e57c5c60abf30ca119c78f3d2/js/tree.html)を制作している。操作の体系は Naruyoko さんのツールとほぼ同じである。 Naruyoko さんには感謝したい。

## 説明

**解析**は、ここでは、外表記による木構造を利用しながら対応関係を帰納的に見つけ出していくようなものを指す。このような解析は、 p進大好きbot さんによる "*[Evaluation of Analysis](https://googology.wikia.org/wiki/User_blog:P%E9%80%B2%E5%A4%A7%E5%A5%BD%E3%81%8Dbot/Evaluation_of_Analysis)*" で Level 3 とされていて、よりよい Level の解析方法が存在することに注意して欲しい。

**項**は、解析の対象となるバシク行列システムなどにおいての項である。

**ノード**は、解析の基本要素である。ある**項** `x` を対象とする。ある**項** `x[n]` を対象とするような**ノード**を子に持つ。一つずつ**メモ**を持つ。**対応の記録**を参照して、解析結果を表示して編集できる。

**対応の記録**は、ある**項**に対して、複数の**ある項へと対応づけられる特定の表記による項**を対応づけるような表である。

**ある項へと対応づけられる特定の表記による項**は、**ある項へと対応づけられる項**に、それがどの表記によるものであるか注釈したものである。

**ある項へと対応づけられる項**は、何らかの表記によって書かれていて、何らかの**項**へと対応づけられる項である。

**メモ**は、解析におけるメモを書き込める所である。

ユーザーが**ノード**を expand することは、その**ノード**の子が `x[0]`, `x[1]`, `x[2]` という `3` 未満までであるとき、 `x[3]` に対応する**ノード**を追加することである。その**ノード**の共終タイプが `t` であって、その**ノード**の子が `n` 未満までであって、 `t ≥ n` であるとき、この操作は出来ない。

ユーザーが**ノード**を retract することは、その**ノード**を削除することである。その**ノード**が、子を持っているか、空でないメモを持つとき、この操作は出来ない。

ユーザーが**ノード**に focus することは、その**ノード**をルートの位置に置くことである。

ユーザーが unfocus することは、ある**ノード**に focus している状態を解除することである。

ユーザーが**メモ**を編集することは、その**メモ**に書き込んだり消したりすることである。

ユーザーが記録をエクスポートすることは、全ての展開された**ノード**と、それらの**メモ**と、**対応の記録**をエクスポートすることである。

ユーザーが記録をインポートすることは、ユーザーがエクスポートした記録をインポートして、それが記録している状態へと、**ノード**や**対応の記録**などを設定することである。
