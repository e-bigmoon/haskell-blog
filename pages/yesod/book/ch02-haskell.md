---
title: Haskell
date: 2018/09/17
---

Haskell は強力で速く、型安全なプログラミング言語です。本書は、Haskell の基礎についてある程度理解している読者を対象としています。Haskell に馴染みが無い読者には、オンラインで読むことができる素晴らしい入門書が2冊あるので、そちらをご紹介します。

- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/read/)

さらに [School of Haskell](https://www.fpcomplete.com/introduction-to-haskell) にも良い記事がいくつもあります。

Yesod を使うためには、少なくとも Haskell の基本的な部分について理解しておく必要があります。また、それだけでなく、入門書では扱わないような Haskell の機能についても Yesod では扱うことになります。この章では、本書で想定している Haskell の基礎知識と読者の知識とのギャップを確認しておこうと思います。

もし、Haskell にすごく詳しければ、この章は完全にスキップしてもらって構いません。また、とりあえず Yesod を触ってみたいと思う人は、後からこの章に戻ってこれば良いので、好きに動かしてみてください。

## ことばについて

自分の手足のように Haskell を扱う人でさえも、Haskell の用語についてはたまに混乱してしまうことがあります。まずは、本書で扱う用語について確認していきましょう。

### データ型

### データコンストラクタ

上記の例だと、`Person`、`Make`、`Bicycle`、`Car` などがデータコンストラクタです。

### 型コンストラクタ

上記の例だと、`Person`、`Make`、`Vehicle` が型コンストラクタです。

### 型変数

`data Maybe a = Just a | Nothing` で考えると、`a` が型変数です。

<div class="yesod-book-notice">
`Person` と `Make` 型はデータコンストラクタと型コンストラクタで同じ名前を利用しています。これはデータコンストラクタが1つしかない場合の良くある慣習のようなものです。言語が強制するものではないので、データコンストラクタと型コンストラクタに異なる名前を付けることもできます。
</div>

## ツール

## 言語拡張

### Overloaded Strings

### Type Families

### Template Haskell

### QuasiQuotes

## API ドキュメント

## まとめ

Yesod を使うために Haskell のエキスパートになる必要はありません。基本的なことについて、ちょっとだけわかっていれば大丈夫です。この章の内容が本書をこれから楽しく読み進めるために役に立つことでしょう。