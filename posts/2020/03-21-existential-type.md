---
title: 存在型に入門しよう！
author: Shinya Yamaguchi
tags: bigmoon
# updated: 2020/02/19
---

TAPL の存在型の章が面白かったので Hakell で解説します。

<!--more-->

## 存在型の基本形

存在型という単語はとても難しい雰囲気がありますが、そんなことはありません。

まずは Haskell で存在型を定義してみましょう。

### ExistentialQuantification を使った定義

`ExistentialQuantification` 言語拡張を使った定義は以下のようになります。存在型の定義方法はこの方法以外にも `GADTs` を使う方法があります。

```haskell
{-# LANGUAGE ExistentialQuantification #-}
data Any = forall a. Any a
```

この型の値もいくつか作ってみましょう。

```hs
a1, a2, a3, a4 :: Any
a1 = Any True
a2 = Any 0
a3 = Any 'a'
a4 = Any "haskell"
```

`a1` ~ `a4` まで全て同じ `Any` 型ですが、データコンストラクタ `Any` の内側の値はすべて異なる型の値になっています。

そのため、この `Any` 型を使えば異なる型を格納するヘテロリストなんかも作れてます。

```hs
list :: [Any]
list = [a1, a2, a3, a4]
```

しかし、このままでは値を定義するだけで使うことができないため、これでは役に立ちません。`Any` 型を少しだけ修正して値を使えるようにしてみましょう。

```hs
data Any = forall a. Any a (a -> Bool)
```

`(a -> Bool)` の関数を追加しました。これで存在型の値に何らかの操作を行えます。
ここでは、false っぽい値に対して `True` を返す関数を、それぞれの型ごとの操作にしましょう。具体的には以下のような感じです。

```hs
a1, a2, a3, a4 :: Any
a1 = Any True not
a2 = Any 0 (==0)
a3 = Any 'a' (==' ')
a4 = Any "haskell" null
```

最後に、`Any` を `Bool` に変換する関数を定義します。

操作するための関数の型を `(a -> Bool)` としたので、ここでは `Bool` 以外には変換できません。

```hs
-- LambdaCase 言語拡張を使っています
elimAny :: Any -> Bool
elimAny = \case
  Any x f -> f x
```

GHCi で確認してみましょう。

```hs
*Main> map elimAny list 
[False,True,False,False]
```

ここまでで、以下の内容について説明が終わりました。

- 存在型の定義方法
- 存在型の値の作り方
- 存在型の値の使い方 (操作方法)

具体例を見てわかる通り、ようするに存在型というのは、「ある特定の型の値」と「その型を操作する方法」の組を値とする型です。

ただ、値を作る時に毎回同じ関数を渡すのは面倒なので、型クラスを使って改良してみます。その前に、今回定義した `a -> Bool` の関数について少しだけ考察してみましょう。

### 有用な型はどれ？



### 操作を型クラスへ

まずは先ほど `Any` の中に埋め込んでいた `(a -> Bool)` の関数を型クラスにしてみいましょう。

```hs
class Falsy a where
  falsy :: a -> Bool
```

そして、`Any` の内部に操作を埋め込む代わりに、型クラス制約を追加します。

```hs
data Any = forall a. Falsy a => Any a
```

あとは `Falsy` 型クラスのインスタンスを定義します。

```hs
instance Falsy Bool where falsy = not
instance Falsy Int  where falsy = (==0)
instance Falsy Char where falsy = (==' ')
-- TypeSynonymInstances と FlexibleInstances を有効にする必要があります。
instance Falsy String where falsy = null
```

最後に `Any` の値と `elimAny` 関数を定義します。

```hs
a1, a2, a3, a4 :: Any
a1 = Any True
a2 = Any @Int 0 -- TypeApplications を有効にする必要があります。
a3 = Any 'a'
a4 = Any "haskell"

list :: [Any]
list = [a1, a2, a3, a4]

elimAny :: Any -> Bool
elimAny = \case
  Any x -> falsy x
```

実際に使ってみましょう。

```hs
*Main> map elimAny list
[False,True,False,False]
```

上手くいきました。これで値を作るたびに同じ関数を渡す必要はなくなりました。

## まとめ

## 参考リソース

- Types and Programming Languages
- Thinking with Types
- [11.4.6. Existentially quantified data constructors][link-1]
- [Haskell/存在量化された型 - wikibooks][link-2]

[link-1]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ExistentialQuantification
[link-2]: https://ja.wikibooks.org/wiki/Haskell/%E5%AD%98%E5%9C%A8%E9%87%8F%E5%8C%96%E3%81%95%E3%82%8C%E3%81%9F%E5%9E%8B