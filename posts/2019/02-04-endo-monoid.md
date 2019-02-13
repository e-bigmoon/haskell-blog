---
title: Endo Monoid
author: Shinya Yamaguchi
tags: bigmoon, monoid
---

## はじめに

[Data.Monoid](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html) に [Endo](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Endo) 型が定義されています。

`Endo` という名前は `自己準同型 (Endomorphism)` に由来します。

```haskell
newtype Endo a = Endo { appEndo :: a -> a }

instance Semigroup a => Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id
```

使い方は簡単。

```haskell
ghci> f = foldMap Endo [(+1), (*2), negate]
ghci> print $ appEndo f 5
-9

ghci> appEndo (Endo ("Hello, " ++) <> mempty <> Endo (++ "!")) "Haskell"
"Hello, Haskell!"

ghci> (appEndo $ foldMap Endo [("Hello, " ++), id, (++ "!")]) "Haskell"
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

`Endo` は意外と色んなところで使える便利なモノイドです。

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
-- Category の定義より f . id == f
= Endo f

  (mempty :: Endo a) <> Endo f
= Endo id <> Endo id
= Endo (id . f)
-- Category の定義より id . f == f
= Endo f
```

### `f . (g . h) == (f . g) . h`

`(.)` の定義

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)
```

証明

```haskell
(LHS)
  f . (g . h)
= \x -> f ((g . h) x)
= \x -> f ((\y -> g (h y)) x)
= \x -> f (g (h x))

(RHS)
  (f . g) . h
= \x -> (f . g) (h x)
= \x -> (\y -> f (g y)) (h x)
= \x -> f (g (h x))
```

### `id . f == f`

`id` の定義

```haskell
id :: a -> a
id x = x
```

証明

```haskell
  id . f
= \x -> id (f x)
= \x -> f x
= f
```

## 例1) Write Endo パターン

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

このような `Writer` と `Endo` を使った実装パターンは [Quick and Easy DSLs with Writer Endo](https://ocharles.org.uk/blog/posts/2013-02-12-quick-dsls-with-endo-writers.html) で紹介されている `Writer Endo` パターンとして知られているようです。

## 例2) データの更新

こんな感じで設定等を更新する際にも使えるかもしれません。

```haskell
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

import Control.Lens
import Data.Extensible
import Data.Monoid

type Person = Record
  '[ "name" >: String
   , "age"  >: Int
   ]

update :: [Person -> Person] -> Person -> Person
update fs = appEndo (foldMap Endo fs)

me :: [Person -> Person]
me =
  [ (& #name .~ "guchi")
  , (& #age .~ 20)
  ]

defaultPerson :: Person
defaultPerson = #name @= "NONAME"
             <: #age  @= 0
             <: nil
```

実行結果

```haskell
λ> stack repl --package extensible --package lens EndoExample.hs

ghci> defaultPerson
name @= "NONAME" <: age @= 0 <: nil

ghci> update me defaultPerson
name @= "guchi" <: age @= 20 <: nil
```

## 例3) パターンマッチの実装

僕はあまり `Endo` モノイドを使いこなせていませんが、良い感じに使えたと思える例としては TAPL 11章でレコードパターンを実装する際です。

レコードのパターンマッチは代入の合成で書くことができるので、`Endo` がちょうどぴったり適用できました。

```haskell
match :: Pattern -> Value -> (Term -> Term)
match (PtVar _ n) v = subst n v
match p@(PtRecord fs) v@(TmRecord fs')
  | isRecordValue v && sameFieldLength p v
      = appEndo $ foldMap (Endo . uncurry match) $ zip (map snd fs) (map snd fs')
  | otherwise = error "match: pattern match failure"
match PtRecord{} _ = error "match: v is not Rrcord"
```

## 参考

- [Monoids Tour](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour)
- [Quick and Easy DSLs with Writer Endo](https://ocharles.org.uk/blog/posts/2013-02-12-quick-dsls-with-endo-writers.html)
- [Endomorphic Composite as a monoid](http://blog.ploeh.dk/2018/04/16/endomorphic-composite-as-a-monoid/)
