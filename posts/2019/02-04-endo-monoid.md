---
title: Endo Monoid
author: Shinya Yamaguchi
tags: bigmoon
---

## はじめに

[Data.Monoid](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html) に [Endo](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Endo) 型が定義されています。

`Endo` という名前は `自己準同型 (Endomorphism)` に由来します。

```haskell
newtype Endo a = Endo { appEndo :: a -> a }

instance Semigroup a => Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid a => Monoid (Dual a) where
  mempty = Endo id
```

使い方は簡単。

```haskell
ghci> f = foldMap Endo [(+1), (*2), negate]
ghci> print $ appEndo f 5
-9

ghci> appEndo (Endo ("Hello, " ++) <> Endo (++ "!")) "Haskell"
"Hello, Haskell!"
```

`appEndo` すると関数が出てくるところがポイントですね。2つ目の評価の流れをざっくり追うとこんな感じです。

```haskll
  appEndo (Endo ("Hello, " ++) <> Endo (++ "!")) "Haskell"
= appEndo (Endo (("Hello, " ++ ) . (++ "!"))) "Haskell"
= ("Hello, " ++ ) . (++ "!") $ "Haskell"
= "Hello, " ++ ("Haskell" ++ "!")
= "Hello, " ++ "Haskell!"
= "Hello, Haskell!"
```

`appEndo` は意外と色んなところで使える便利なモノイドです。

<!--more-->

## Semigroup, Monoid law の確認

Semigroup Law

```haskell
  Endo f <> (Endo g <> Endo h)
= Endo f <> Endo (g . h)
= Endo (f . (g . h))
-- Category の定義より f . (g . h) == (f . g) . h
= Endo ((f . g) . h)
= Endo (f . g) <> Endo h
= (Endo f <> Endo g) <> Endo h
```

Monoid Law

```haskell
  Endo f <> (mempty :: Endo a)
= Endo f <> Endo id
= Endo (f . id)
-- Category の定義より f . id = f
= Endo f

  (mempty :: Endo a) <> Endo f
= Endo id <> Endo id
= Endo (id . f)
-- Category の定義より id . f = f
= Endo f
```

## Yesod

[yesod-core](https://hackage.haskell.org/package/yesod-core) パッケージの [ProvideRep](https://hackage.haskell.org/package/yesod-core-1.6.11/docs/Yesod-Core-Handler.html#t:ProvidedRep) を扱う関数は `Endo` を利用しています。

```haskell
selectRep :: MonadHandler m => Writer (Endo [ProvidedRep m]) () -> m TypedContent
provideRep :: (Monad m, HasContentType a) => m a -> Writer (Endo [ProvidedRep m]) ()
provideRepType :: (Monad m, ToContent a) => ContentType -> m a -> Writer (Endo [ProvidedRep m]) ()
```

また `Yesod` には [GHState](https://hackage.haskell.org/package/yesod-core-1.6.11/docs/Yesod-Core-Types.html#t:GHState) 型がありますが、そこでも `Endo` を使っています。

```haskell
data GHState = GHState
    { ghsSession :: !SessionMap
    , ghsRBC     :: !(Maybe RequestBodyContents)
    , ghsIdent   :: !Int
    , ghsCache   :: !TypeMap
    , ghsCacheBy :: !KeyedTypeMap
    , ghsHeaders :: !(Endo [Header])
    }
```

## 参考

- [Monoids Tour](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour)
- [Quick and Easy DSLs with Writer Endo](https://ocharles.org.uk/blog/posts/2013-02-12-quick-dsls-with-endo-writers.html)
- [Endomorphic Composite as a monoid](http://blog.ploeh.dk/2018/04/16/endomorphic-composite-as-a-monoid/)