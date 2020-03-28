---
title: 入門Haskellプログラミング
published: 2020/03/27
# updated: 2020/03/27
---

<img src="/images/books/gpwh.jpg" alt="入門Haskellプログラミング&Get Programming with Haskell 表紙" width="300px">

書籍名           | 発売日  | 発売元    | その他
-----------------|--------|-----------|----------
入門Haskellプログラミング | 2019/07/31 | 翔泳社 | [出版社サポートページ][ja-support]<br>[正誤表][ja-errata]<br>[Amazon レビュー][ja-review]
Get Programming with Haskell | 2018/04/02 | Manning Publications | [出版社サポートページ][en-support]<br>[正誤表][en-errata]<br>[Amazon レビュー][en-review]

## レビュー (日本語訳)

まだ読んでいません。

## レビュー (原著)

全600ページなので「[RWH](./real-world-haskell.html)」と同じぐらい分厚いです。ただ各章 (Unit) はそれほど分量があるわけではないので、毎日コツコツ進めれば2ヶ月ぐらいで読み終わることができます。

ビルドツール **stack** の使い方や良く使うパッケージ (**text**, **bytestring**, **aeson**, **http-conduit**) の解説が丁寧に書かれているため最後まで読めば、かなり **Haskell** を実用的に使えるようになる一冊だと思います。

特に他の手続き型言語や、オブジェクト指向言語に慣れている人にとっては身近な例が多いため、ちゃんと学びたい人は最初の1冊目でも良いと思います。今までのアカデミックな雰囲気の **Haskell** に抵抗がある人にオススメできます。

本書の面白い点として `$` や `.` の解説はほとんど行いません。たぶん実用的には無くても困らないからだと思います。

最近の書籍では **Semigroup**, **Monoid**, **Functor**, **Applicative**, **Monad** 型クラスについて必ず言及されるようになってきています。本書では **Semigroup**, **Monoid** の説明がわかりやすくて、誰かに教える際には、僕も同じ例を使ってみようと思います。

また、他の書籍では見られない特徴としては **Maybe** の重要性について、しっかりと具体例を交えて説明している点です。本書全体を通して **Maybe** 型がなぜ重要なのか理解できることでしょう。

個人的に面白いと思った章です。

- Lesson17: Design by composition-Semigroups and Monoids
- Lesson22: Interacting with the command line and lazy I/O
- Lesson23: Working with text and Unicode
- Lesson42: Efficient, stateful arrays in Haskell

モナド変換子や **lens** についての解説は特にありません。また、各単元に練習問題がいくつかついていますが、簡単なのであまり面白く無いかもしれません。

量は多いですが、情報はしっかりと体系的にまとまっているため、入門書としてオススメしても良いのではないかと思います。

[ja-support]: https://www.shoeisha.co.jp/book/detail/9784798158662
[ja-errata]: https://www.shoeisha.co.jp/book/detail/9784798158662
[ja-review]: https://www.amazon.co.jp/product-reviews/4798158666

[en-support]: https://www.manning.com/books/get-programming-with-haskell
[en-errata]: https://www.manning.com/downloads/1792
[en-review]: https://www.amazon.com/product-reviews/1617293768
