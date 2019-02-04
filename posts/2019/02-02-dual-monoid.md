---
title: Dual Monoid
author: Shinya Yamaguchi
tags: bigmoon, monoid
---

## はじめに

[Data.Monoid](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html) に [Dual](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Dual) 型が定義されています。

```haskell
newtype Dual a = Dual { getDual :: a }

instance Semigroup a => Semigroup (Dual a) where
  Dual a <> Dual b = Dual (b <> a)

instance Monoid a => Monoid (Dual a) where
  mempty = Dual mempty
```

使い方は簡単。

```haskell
ghci> getDual $ Dual "aaa" <> Dual "bbb"
"bbbaaa"
```

いつか使う時もあるかもしれないので、適当な例を作ってみます。

<!--more-->

## Semigroup, Monoid law の確認

Semigroup Law

```haskell
  Dual a <> (Dual b <> Dual c)
= Dual a <> Dual (c <> b)
= Dual ((c <> b) <> a)
-- a, b, c は Semigroup なので
= Dual (c <> (b <> a))
= Dual (b <> a) <> Dual c
= (Dual a <> Dual b) <> Dual c
```

Monoid Law

```haskell
  Dual a <> (mempty :: Dual a)
= Dual a <> Dual (mempty :: a)
= Dual ((mempty :: a) <> a)
-- a は Monoid なので
= Dual a

  (mempty :: Dual a) <> Dual a
= Dual (mempty :: a) <> Dual a
= Dual (a <> (mempty :: a))
-- a は Monoid なので
= Dual a
```

## headMaybe

```haskell
headMaybe :: [a] -> Maybe a
headMaybe = getLast . getDual . foldMap (Dual . Last . pure)

-- First を使った定義
headMaybe :: [a] -> Maybe a
headMaybe = getFirst . foldMap (First . pure)
```

```haskell
ghci> headMaybe []
Nothing

ghci> headMaybe "abcd"
Just 'a'
```

## lastMaybe

```haskell
lastMaybe :: [a] -> Maybe a
lastMaybe = getFirst . getDual . foldMap (Dual . First . pure)

-- Last を使った定義
lastMaybe :: [a] -> Maybe a
lastMaybe = getLast . foldMap (Last . pure)
```

```haskell
ghci> lastMaybe []
Nothing

ghci> lastMaybe "abcd"
Just 'd'
```

## last

```haskell
last' :: [a] -> a
last' = getAlt . getDual . foldMap (Dual . Alt . pure)
```

```haskell
ghci> last []
*** Exception: user error (mzero)

ghci> last "abc"
'c'
```

## reverse

```haskell
rev :: [a] -> [a]
rev = getDual . foldMap (Dual . pure)
```

```haskell
ghci> rev [5,2,4,7]
[7,4,2,5]

ghci> rev "Hello World"
"dlroW olleH"
```

## foldl

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
```

## 参考

- [Folding a structure in reverse](https://riptutorial.com/haskell/example/2555/folding-a-structure-in-reverse)
- [Five Minutes to Monoid](https://medium.com/@sjsyrek/five-minutes-to-monoid-fe6f364d0bba)
- [Monoids: Theme and Variations (Functional Pearl)](http://ozark.hendrix.edu/~yorgey/pub/monoid-pearl.pdf)
- [Foldable](https://www.stackage.org/haddock/lts-13.5/base-4.12.0.0/Prelude.html#t:Foldable)