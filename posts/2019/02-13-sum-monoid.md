---
title: Sum Monoid
author: Shinya Yamaguchi
tags: bigmoon, monoid, package
---

## はじめに

[Data.Monoid](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html) に [Sum](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Sum) 型が定義されています。

```haskell
newtype Sum a = Sum { getSum :: a }

instance Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
```

使い方は簡単。

```haskell
ghci> getSum (Sum 1 <> Sum 2 <> mempty)
3

ghci> getSum $ foldMap Sum [1..10]
55
```

<!--more-->

## Semigroup, Monoid law の確認

Semigroup Law

```haskell
  Sum a <> (Sum b <> Sum c)
= Sum a <> Sum (b + c)
= Sum (a + (b + c))
-- (+) の結合律より
= Sum ((a + b) + c)
= Sum (a + b) <> Sum c
= (Sum a <> Sum b) <> Sum c
```

Monoid Law

```haskell
  Sum a <> (mempty :: Sum a)
= Sum a <> Sum 0
= Sum (a + 0)
= Sum a

  (mempty :: Sum a) <> Sum a
= Sum 0 + Sum a
= Sum (0 + a)
= Sum a
```

## newtype を使って定義する理由

わざわざ `Sum` という新しい型を作ってインスタンスを定義しなくても、`Int` を使って直接 `Semigroup` と `Monoid` のインスタンスを定義すれば良いんじゃないか？と普通は思います。

実際に以下の定義は問題なく動きます。

```haskell
instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0
```

```haskell
ghci> 1 <> 2 <> mempty :: Int
3

ghci> mconcat [1..10] :: Int
55
```

しかし、足し算と同様に掛け算もモノイドとして定義したいと思う人もいるでしょう。

この時 `Int` は既に `Semigroup`, `Monoid` のインスタンス定義があるため、同じファイルに以下のように定義することはできません。

```haskell
instance Semigroup Int where
  (<>) = (*)

instance Monoid Int where
  mempty = 1
```

この問題を解決する方法はいくつかありますが、このような例では `newtype` が適切ということです。

## Law の自動チェック (quickcheck-classes)

[quickcheck-classes](https://hackage.haskell.org/package/quickcheck-classes) を使って `Semigroup law` と `Monoid law` がちゃんと法則を満たすかどうかチェックしてみます。

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Sum a = Sum { getSum :: a }
  deriving (Eq, Show, Arbitrary)

instance Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
```

`Eq`, `Show`, `Arbitrary` が無いとチェックできないので `derive` しています。

実行はめっちゃ簡単で、`lawsCheck` 関数と `semigroupLaws` や `monoidLaws` などのチェックしたい則に対応する関数を使うだけです。

```shell
> import Test.QuickCheck.Classes
> import Data.Proxy

> lawsCheck (semigroupLaws (Proxy :: Proxy (Sum Int)))
Semigroup: Associative +++ OK, passed 100 tests.
Semigroup: Concatenation +++ OK, passed 100 tests.
Semigroup: Times +++ OK, passed 100 tests; 99 discarded.

> lawsCheck (monoidLaws (Proxy :: Proxy (Sum Int)))
Monoid: Associative +++ OK, passed 100 tests.
Monoid: Left Identity +++ OK, passed 100 tests.
Monoid: Right Identity +++ OK, passed 100 tests.
Monoid: Concatenation +++ OK, passed 100 tests.
```

参考までに `mempty` の定義が Law を満たさない場合の例も載せておきます。

```haskell
instance Num a => Monoid (Sum a) where
  mempty = Sum 1
```

```shell
> import Test.QuickCheck.Classes
> import Data.Proxy

> lawsCheck (semigroupLaws (Proxy :: Proxy (Sum Int)))
Semigroup: Associative +++ OK, passed 100 tests.
Semigroup: Concatenation +++ OK, passed 100 tests.
Semigroup: Times +++ OK, passed 100 tests; 113 discarded.

> lawsCheck (monoidLaws (Proxy :: Proxy (Sum Int)))
Monoid: Associative +++ OK, passed 100 tests.
Monoid: Left Identity *** Failed! Falsifiable (after 1 test):
  Description: mappend mempty a = a
  a = Sum {getSum = 0}
  mappend mempty a = Sum {getSum = 1}
Monoid: Right Identity *** Failed! Falsifiable (after 1 test):
  Description: mappend a mempty = a
  a = Sum {getSum = 0}
  mappend a mempty = Sum {getSum = 1}
Monoid: Concatenation +++ OK, passed 100 tests.
```

こんな感じでモノイドの左単位元則 (Left Identity law) と右単位元則 (Right Identity law) が満たされていないことを教えてくれます。

## 参考

- [Lists and Other Monoids](https://people.cs.kuleuven.be/~tom.schrijvers/Research/talks/lhug4.pdf)