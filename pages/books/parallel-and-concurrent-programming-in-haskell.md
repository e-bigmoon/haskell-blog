---
title: Haskell による並列・並行プログラミング
published: 2020/03/27
updated: 2020/08/26
---

<img src="/images/books/pc.jpg" alt="Haskell による並列・並行プログラミング 表紙" width="300px">

書籍名           | 発売日  | 発売元    | その他
---------------|------|--------|----------
Haskell による並列・並行プログラミング | 2014/08/21 | オライリージャパン | [出版社サポートページ][ja-support]<br>[正誤表][ja-errata]<br>[Amazon レビュー][ja-review]
Parallel and Concurrent Programming in Haskell | 2013/07/12 | O'Reilly Media | [出版社サポートページ][en-support]<br>[著者サポートページ][en-support2]<br>[正誤表][en-errata]<br>[Amazon レビュー][en-review]

## レビュー (日本語訳)

通称: ヘイヘイHaskell

現在は **Facebook** にいる **Haskell** 界の有名人 **Simon Marlow** さんの並列・並行本です。

2020年に読みました。書籍のプログラムは[github][parconc-examples-github]で管理されており、少し修正することで手元の環境でも9割以上のプログラムが実際に動作することを確認できました。

書籍で扱われているトピックはどれも高度で難しいので、最後まで読み切るのは大変だと思います。

- Haskell で並列・並行・分散プログラムを書いてみたい。
- 並列・並行・分散プログラムを書きたいけど、どう書いたら良いのかわからない。
- [accelerate][hkg-accelerate], [async][hkg-async], [stm][hkg-stm] などの理解を深めたい。

という人向けの内容になっています。

並列・並行プログラムを作って終わりではなく、通常のプログラムから少しずつ並列・並行プログラムに直していくスタイルなのでわかりやすく、ちゃんとベンチマークやプロファイル結果を元に議論を進めている点もオススメポイントです。

[threadscope][threadscope-github] や [ghc-events][ghc-events] の基本的な使い方、プロファイリングの読み方、GHC のランタイムオプションなどについても丁寧に解説されています。

個人的には特に

- **Repa**, **accelerate** を利用した並列処理
- 非同期通信や非同期例外
- **MVar** と **STM** の本質的な振る舞いと性能の違い

などについて、本書で理解を深めることができました。

他の **Haskell** 書籍には書かれていないオリジナルトピック満載なので是非、実際にプログラムを動かしながら読んでみてください。

## 関連リンク

- [parconc-examples](https://hackage.haskell.org/package/parconc-examples)
- [simonmar/parconc-examples][parconc-examples-github]
- [waddlaw/book-pcph](https://github.com/waddlaw/book-pcph)

[ja-support]: https://www.oreilly.co.jp/books/9784873116891/
[ja-errata]: https://www.oreilly.co.jp/books/9784873116891/
[ja-review]: https://www.amazon.co.jp/product-reviews/4873116899/

[en-support]: https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/
[en-support2]: https://simonmar.github.io/pages/pcph.html
[en-errata]: https://www.oreilly.com/catalog/errata.csp?isbn=0636920026365
[en-review]: https://www.amazon.com/product-reviews/B00DWJ1BIG/

[threadscope-github]: https://github.com/haskell/ThreadScope
[ghc-events]: https://github.com/haskell/ghc-events
[hkg-async]: https://hackage.haskell.org/package/async
[hkg-stm]: https://hackage.haskell.org/package/stm
[hkg-accelerate]: https://hackage.haskell.org/package/accelerate

[parconc-examples-github]: https://github.com/simonmar/parconc-examples