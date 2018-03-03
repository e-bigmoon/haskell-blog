---
title: Liquid Haskell
author: Shinya Yamaguchi
tags: bigmoon, liquidhaskell
---

## はじめに

Liquid Haskell で少しハマったのでメモとして残しておきます。

本来なら先に仕様を書いて実装を書くべきだと思いますが、今回の例は既存のコードにリファインメント型をつけるような場合を想定しています。

```shell
$ liquid
LiquidHaskell Version 0.8.2.4, Git revision d641244775cd842776cecf2c5d3e9afa01549e76 (dirty)
Copyright 2013-18 Regents of the University of California. All Rights Reserved.
```

Liquid Haskell を気になってる人向けの記事です。

<!--more-->

## やりたいこと

データの挿入と更新操作を次のような型で表現します。

```haskell
data Operation = Insert | Update
  deriving Eq
```

上記のデータ型を使って、次のような関数を定義します。

```haskell
adjustBound :: Operation -> Int -> Int -> Int -> Int
adjustBound op lower upper n | isInsert op = upper + 1
                             | otherwise   = lower `max` (n `min` upper)

isInsert :: Operation -> Bool
isInsert Insert = True
isInsert _      = False
```

`adjustBound` 関数は以下のように動作します。

```haskell
> adjustBound Insert 0 10 5
11
> adjustBound Insert 0 10 100
11
> adjustBound Insert 0 10 (-100)
11

> adjustBound Update 0 10 5
5
> adjustBound Update 0 10 100
10
> adjustBound Update 0 10 (-100)
0
```

`adjustBound` のような関数でバグが無いことを確認するためには何をしたら良いでしょうか？

型は非常に強力ですが、値について何も教えてくれません。

## バグの少ない世界を目指して

僕が Haskell を使う理由は、第一に `楽しい` からです。そのため、「勉強しても就職する時に役に立たないでしょ？」などと言われても全く気になりません。(そもそも、就職するために勉強するわけじゃないですよね)

また Haskell を使えば、正しいソフトウェアを普通に作ることができます。また、`hspec` などで単体テストを書いたり、`QuichCheck` などでランダムテストを書くことで、過去に起こった問題を再発させないようにする努力や、バグを少なくするための取り組みが行われています。

しかしながら、個人的にはどれもまだ不安です。もしかしたら、チェックしてない部分にバグがあるんじゃないの・・・？

そんな心配性の方は `Liquid Haskell (LH)` を使いましょう！

## 型をより厳しく

最初に定義した `Operation` 型と `adjustBound` を再掲します。

```haskell
-- LH.hs
module LH where

data Operation = Insert | Update
  deriving Eq

adjustBound :: Operation -> Int -> Int -> Int -> Int
adjustBound op lower upper n | isInsert op = upper + 1
                             | otherwise   = lower `max` (n `min` upper)

isInsert :: Operation -> Bool
isInsert Insert = True
isInsert _      = False
```

とりあえず、現在のコードを `LH` にかけてみます。

```shell
$ liquid LH.hs
LiquidHaskell Version 0.8.2.4, Git revision d641244775cd842776cecf2c5d3e9afa01549e76 (dirty)
Copyright 2013-18 Regents of the University of California. All Rights Reserved.


**** DONE:  A-Normalization ****************************************************


**** DONE:  Extracted Core using GHC *******************************************


**** DONE:  Transformed Core ***************************************************

Working 100% [=================================================================]

**** DONE:  annotate ***********************************************************


**** RESULT: SAFE **************************************************************
```

`RESULT: SAFE` が表示されれば問題ありません！

### 入力を自然数に限定させよう

例えば `lower` と `upper` が自然数 (0含む) しか許容しないという仕様が与えられた時、どうしますか？

よくある対応としては、コメントにその旨を書いたり、テストを作ったりという作業になるでしょう。

`Liquid Haskell` では上記の仕様を `事前条件` として記述することができます。

```haskell
{-@ adjustBound :: _ -> Nat -> Nat -> _ -> _ @-}
adjustBound :: Operation -> Int -> Int -> Int -> Int
adjustBound op lower upper n | isInsert op = upper + 1
                             | otherwise   = lower `max` (n `min` upper)
```

`Nat` は [Prelude](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Prelude.spec) で以下のように定義されています。つまり、0以上の `Int` のみを含むリファインメント型です。

```haskell
{-@ type Nat = {v: Int | v >= 0 } @-}
```

これだけです。`LH` で結果を確かめてみましょう。

```shell
$ liquid LH.hs
...

**** RESULT: SAFE **************************************************************
```

`SAFE` ですね！

これでもう `adjustBound` の `lower` と `upper` は `0` 以上の自然数でしか呼び出されていないことが示されました。

### もう少し具体例

では、別のプログラマが `adjustBound` を利用した関数を作ったとしましょう。この関数自体に意味はないですが、`LH` を理解するためにはとても良い例だと思います。

```haskell
f :: Int
f = adjustBound Insert (-100) (-50) (-70)
```

この関数 `f` は、型が正しいため当然コンパイルできます。

```shell
$ stack repl -- LH.hs
> f
-49
```

けれども、僕らの仕様では `adjustBound` の `lower` と `upper` には自然数しか適用してはいけないはずです。

次に `LH` を実行してみましょう。

```shell
$ liquid LH.hs
**** RESULT: UNSAFE ************************************************************


 LH.hs:18:25-28: Error: Liquid Type Mismatch

 18 | f = adjustBound Insert (-100) (-50) (-70)
                              ^^^^


   Inferred type
    VV : {v : Int | v == (-?a)
                    && v == ?b}

  not a subtype of Required type
    VV : {VV : Int | VV >= 0}

  In Context
    ?b : {?b : Int | ?b == (-?a)}

    ?a : {?a : Int | ?a == (100 : int)}


 LH.hs:18:32-34: Error: Liquid Type Mismatch

 18 | f = adjustBound Insert (-100) (-50) (-70)
                                     ^^^


   Inferred type
    VV : {v : Int | v == (-?b)
                    && v == ?a}

  not a subtype of Required type
    VV : {VV : Int | VV >= 0}

  In Context
    ?b : {?b : Int | ?b == (50 : int)}

    ?a : {?a : Int | ?a == (-?b)}
```

`UNSAFE` になりましたね。こういうことです。

つまり、**自分たちが使っている範囲**で `Liquid Haskell` のリファインメント型について、正しく整合性が取れているのかということを判定しています。

### 戻り値の型も厳しくしよう！

先程、事前条件についてリファインメント型を書きました。

次は事後条件についてリファインメントを書きましょう！

同様に戻り値の型も自然数という仕様にします。

```haskell
{-@ adjustBound :: _ -> Nat -> Nat -> _ -> Nat @-}
adjustBound :: Operation -> Int -> Int -> Int -> Int
adjustBound op lower upper n | isInsert op = upper + 1
                             | otherwise   = lower `max` (n `min` upper)
```

```shell
$ liquid LH.hs
**** RESULT: SAFE **************************************************************
```

リファインメント型 (Refinement type) は `篩 (ふるい) 型` と訳されている本 ([入門LiquidHaskell−篩型による静的コード解析−
](https://taimen.jp/f/389)) もありますが、それは `Haskell` の型の値が条件によって `ふるい` 落とされて、新しい型 (リファインメント型) になっているというイメージから来ているのだと思います。(読んだこと無いので間違ってたらすみません・・・。)

追記: チェシャ猫さんから `篩型` について教えてもらいました！

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">&gt; リファインメント型 (Refinement type) は 篩 (ふるい) 型 と訳されている本もありますが<br><br>頒布したときに最も多かった質問は「これ何て読むんですか？」だった。ちなみに「篩型」はこの本で勝手に作った造語ではなく、論文タイトルなどにも使われています。<a href="https://t.co/Du6mK1hqdD">https://t.co/Du6mK1hqdD</a></p>&mdash; チェシャ猫 (@y_taka_23) <a href="https://twitter.com/y_taka_23/status/969499842895495168?ref_src=twsrc%5Etfw">2018年3月2日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet" data-lang="ja"><p lang="ja" dir="ltr">五十嵐先生と末永先生が発案した訳語のようです。 <a href="https://t.co/rcZuFuptl6">https://t.co/rcZuFuptl6</a></p>&mdash; チェシャ猫 (@y_taka_23) <a href="https://twitter.com/y_taka_23/status/969515905737621506?ref_src=twsrc%5Etfw">2018年3月2日</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>


## もっと仕様を

`adjustBound` 関数はこれで十分なのでしょうか？人によっては十分だね。と答えるかもしれません。

しかし、今回は次のような仕様を与えることにします。

1. `upper` は `lower` 以上の自然数
1. `Insert` の操作の場合の戻り値は `lower` 〜 `upper + 1` の間の自然数
1. `Update` の操作の場合の戻り値は `lower` 〜 ``lower `max` (n `min` upper)`` の間の自然数

ここからが面白いところです。

まずは前準備として `x 〜 y` までの間の自然数を表すリファインメント型と述語を定義します。

```haskell
{-@ type Btwn Lo Hi = {v:Int | Lo <= v && v <= Hi} @-}
{-@ predicate BtwnP Lo Hi = Lo <= v && v <= Hi @-}
```

では、`仕様1`を反映させてみましょう。

```haskell
{-@ adjustBound :: _ -> l:Nat -> {u:Nat | l <= u} -> _ -> Nat @-}
adjustBound :: Operation -> Int -> Int -> Int -> Int
adjustBound op lower upper n | isInsert op = upper + 1
                             | otherwise   = lower `max` (n `min` upper)
```

```shell
$ liquid LH.hs
**** RESULT: SAFE **************************************************************
```

では次に、`仕様2` と `仕様3` です。

リファインメント型は以下のようになります。

```haskell
{-@ adjustBound ::
      op:Operation ->
      l:Nat ->
      {u:Nat | l <= u} ->
      _ ->
      {v:Nat | if (isInsert op) then (BtwnP l (u+1)) else BtwnP l u }
@-}
adjustBound :: Operation -> Int -> Int -> Int -> Int
adjustBound op lower upper n | isInsert op = upper + 1
                             | otherwise   = lower `max` (n `min` upper)

{-@ measure isInsert @-}
isInsert :: Operation -> Bool
isInsert Insert = True
isInsert _      = False
```

```shell
$ liquid LH.hs
**** RESULT: SAFE **************************************************************
```

## はまったポイント

`if (isInsert op) then (BtwnP l (u+1)) else BtwnP l u` の部分でかなりはまりました。

例えば `if` の括弧を外した場合は次のようなエラーになります。

`if isInsert op then (BtwnP l (u+1)) else BtwnP l u`

```shell
**** RESULT: ERROR *************************************************************


 LH.hs:10:73: Error: Cannot parse specification:

 10 | {-@ adjustBound :: op:Operation -> l:Nat -> {u:Nat | l <= u}  -> _ -> {v:Nat | if isInsert op then (BtwnP l (u+1)) else BtwnP l u } @-}
                                                                              ^

     unexpected ":"
     expecting operator, white space or "}"
```

また、同様に `then` の括弧を外してもエラーになります。

`if (isInsert op) then BtwnP l (u+1) else BtwnP l u`

```shell
**** RESULT: ERROR *************************************************************


 LH.hs:10:73: Error: Cannot parse specification:

 10 | {-@ adjustBound :: op:Operation -> l:Nat -> {u:Nat | l <= u}  -> _ -> {v:Nat | if (isInsert op) then BtwnP l (u+1) else BtwnP l u } @-}
                                                                              ^

     unexpected ":"
     expecting operator, white space or "}"
```

`else` については括弧があっても無くても `SAFE` です。

この挙動が本当にわからなくてつらかったです・・・。

ちなみに、以下のような場合も同様にはまるので、ご注意ください。

```
-- UNSAFE
{-@ adjustBound :: _ -> l:Nat -> {u:Nat | l <= u}  -> _ -> Btwn l (u+1) @-}
{-@ adjustBound :: _ -> l:Nat -> {u:Nat | l <= u}  -> _ -> Btwn l {u+1} @-}

-- SAFE
{-@ adjustBound :: _ -> l:Nat -> {u:Nat | l <= u}  -> _ -> Btwn {l} {u+1} @-}
```

## まとめ

- `if` を使う場合は多めに括弧を付けておいた方が良さそう。
- `{}` で囲むと上手くいく場合もある
- `LiquidHaskell` はすごい

この良くわからない挙動について一緒に考えてくれた友人の tkg さんありがとうございました。

以上です。