---
title: extensible-0.6.1 マイグレーションガイド
author: Shinya Yamaguchi
tags: bigmoon, monoid, package
---

## はじめに

[extensible](https://hackage.haskell.org/package/extensible) のバージョンを最新の **0.6.1** に更新した際にいくつかコードの修正が必要になったので、メモ程度に残しておきます。

今回の記事では一部の変更点にしか触れないため、完全な変更点については [CHANGELOG](https://github.com/fumieval/extensible/blob/master/CHANGELOG.md) を参照してください。

### 0.5

- [Cont'](https://hackage.haskell.org/package/extensible-0.4.10.1/docs/Data-Extensible-Wrapper.html#t:Const-39-) が削除されました。

今後は [Control.Applicative.Const](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#t:Const) を利用しましょう。

### 0.5.1

- [membership](https://hackage.haskell.org/package/membership) パッケージが依存関係に追加されました。(`Data.Extensible.HList` と `Data.Extensible.Internal` が membership パッケージに移動した形です)
- `AssocKey`, `AssocValue`, `ValueIs`, `KeyValue` が廃止予定になりました。

今後はそれぞれ以下の型を利用しましょう。

0.5.1 より前 | 0.5.1 以降
------------|------------
[AssocKey](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#t:AssocKey) | [KeyOf](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#t:KeyOf)
[AssocValue](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#t:AssocValue) | [TargetOf](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#t:TargetOf)
[ValueIs](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#t:ValueIs) | [TargetIs](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#t:TargetIs)
[KeyValue](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#t:KeyValue) | [KeyTargetAre](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#t:KeyTargetAre)

- `proxyAssocKey`, `stringAssocKey`, `proxyAssocValue` も同様に廃止予定になりました。

今後はそれぞれ以下の関数を利用しましょう。

0.5.1 より前 | 0.5.1 以降
------------|------------
[proxyAssocKey](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#v:proxyAssocKey) | [proxyKeyOf](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#v:proxyKeyOf)
[stringAssocKey](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#v:stringAssocKey) | [stringKeyOf](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#v:stringKeyOf)
[proxyAssocValue](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#v:proxyAssocValue) | [proxyTargetOf](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Field.html#v:proxyTargetOf)

- [Associate](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Class.html#t:Associate) が廃止予定になりました。

今後は [Lookup](https://hackage.haskell.org/package/extensible-0.5.1/docs/Data-Extensible-Class.html#t:Lookup) を利用しましょう。

引数の順番が変わる点に注意してください。そのままではコンパイルエラーになります。

```haskell
type Associate k v xs = Lookup xs k v
```

### 0.6

- `(:*)` と `(:|)` がそれぞれ廃止予定になりました。

今後はそれぞれ以下の関数を利用しましょう。その際、引数の順番が逆になっているので注意してください。

0.6 より前 | 0.6 以降
----------|-----------
[(:*)](https://hackage.haskell.org/package/extensible-0.6/docs/Data-Extensible-Struct.html#t::-42-) | [(:&)](https://hackage.haskell.org/package/extensible-0.6/docs/Data-Extensible-Struct.html#t::-38-)
[(:|)](https://hackage.haskell.org/package/extensible-0.6/docs/Data-Extensible-Sum.html#t::-124-) | [(:/)](https://hackage.haskell.org/package/extensible-0.6/docs/Data-Extensible-Sum.html#t::-47-)

```haskell
type (:*) h xs = xs :& h
type (:|) h xs = xs :/ h
```

- [TangleT](https://hackage.haskell.org/package/extensible-0.6/docs/Data-Extensible-Tangle.html#t:TangleT) の引数の順番が入れ替わりました。

```haskell
-- 0.6 より前の定義
newtype TangleT h xs m a

-- 0.6 以降の定義
newtype TangleT xs h m a
```

### 0.6.1

- `0.5.1` で廃止予定となっていた `deriveIsRecord` が削除されました。

今後は `Generics` のインスタンスであれば導出可能です。

```haskell
-- 0.6.1 より前
deriveIsRecord ''Foo

-- 0.6.1 以降の定義 (Foo が Generics のインスタンスであればOK)
instance IsRecord Foo
```

## 終わりに

`0.6.1` に一気に更新しましたが、特にハマることも無くスムーズに更新が完了しました。

いくつか新しい関数などが追加されているので、その辺りも要チェックですね。

## 宣伝

[技術書典7](https://techbookfest.org/event/tbf07)に参加します。

![サークルカット](/images/2019/08-19/circle.png)

現時点で Haskell 本を2冊 (1人1冊) 執筆しています。(値段や配布数については未定です)

### (1) すごいHaskell自分で作ろう！ Write Your Haskell for Great Good!

![表紙](/images/2019/08-19/main.jpg)

【著者】
@gotoki_no_joe

【概要】
Haskell でプログラミングするのは楽しい。ところで「Haskell*を*プログラミングする」のも楽しいのでは？じゃあやってみよう。というわけで、Haskell からヒラヒラを全て削り落して削りすぎたような関数型言語を作ります。

構文を設計し、意味を定義し、それらに従ってインタプリタを **Haskell** と **TypeScript** で実装します。

Haskell といいつつ普通でわかりやすい正格評価な処理系から始めて、その後で遅延評価をする処理系に取り組みます。必要になった部分だけを計算する遅延評価の仕組みが、組み込み演算やパターンマッチと協調する様子を自作して理解します。

おまけに TypeScript によるパーサコンビネータの作り方が付いています。

ごめんなさい！Haskell らしさの重要な柱である型システムについて執筆が間に合いませんでした。インデント構文、JavaScript を出力するコンパイラとともに、後編での執筆を予定しています。

【目次 (仮)】

![目次1](/images/2019/08-19/toc1.png)
![目次2](/images/2019/08-19/toc2.png)

### (2) GHC API 入門 (仮)

【著者】
@waddlaw

【概要】
GHC API を使って具体的に動く何かを作る予定ですが、全然筆が進んでいないので内容未定・・・。(上記の書籍と比べると宣伝できるものが何も・・・)

頑張ります！

### まとめ

Haskell の処理系を作ってみたいなーって思っている人は「すごいHaskell自分で作ろう！ Write Your Haskell for Great Good!」がおすすめです！！！

`TypeScript` と `Haskell` という異なるパラダイムの言語による実装が一冊に収まっているので、とてもお得ですね。

応援よろしくお願いします〜。
