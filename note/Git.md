# Git

commit は、 `git add -p` を使って可能な限り細かくする。ただし、変更を一塊として捉えることが出来たり複雑に絡み合っていたりする時は無理に分けない。

`git add -p` を使って commit する時は、一回 commit するたびに `git push` をして CI にかける。

commit のメッセージのタイトルは、最初に `Add` や `Update` や `Fix` や `Clean` などの commit の性質を表すコードを書く。その後にコロンを挟んで commit の変更内容を端的に書く。たとえば、 `Add: Case モジュールを` のようになる。

commit のメッセージの本文は、その commit を行った理由や、その commit の内容を author がそうした理由などを書く。さらに、メモなどを書いても良い。
