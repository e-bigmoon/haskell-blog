---
title: First Monoid
author: Shinya Yamaguchi
tags: bigmoon, monoid
---

## はじめに

[Data.Monoid](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html) に [First](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:First) 型が定義されています。

```haskell
newtype First a = First { getFirst :: Maybe a }

instance Semigroup (First a) where
  First Nothing <> b = b
  a             <> _ = a

instance Monoid (First a) where
  mempty = First Nothing
```

`First` モノイドは `Endo` や `Dual` モノイドとは定義の雰囲気が少し違いますね。

- `getFirst :: a` ではなく `getFirst :: Maybe a` となっている
- インスタンス宣言にクラス制約が必要無い

使い方は簡単。

```haskell
ghci> getFirst (First (Just "hello") <> First Nothing <> First (Just "world"))
Just "hello"
```

常に最初の値を返すという、一見意味の無さそうな `First` モノイドも実は便利に使えたりします。

<!--more-->

## Semigroup law の確認

### Case (1) (a == Nothing, b == Nothing)

```haskell
  First a <> (First b <> First c)
= First b <> First c
= First c
```

```haskell
  (First a <> First b) <> First c
= First b <> First c
= First c
```

### Case (2) (a == Nothing, b == Just b')

```haskell
  First a <> (First b <> First c)
= First b <> First c
= First b
```

```haskell
  (First a <> First b) <> First c
= First b <> First c
= First b
```

### Case (3) (a == Just a')

```haskell
  First a <> (First b <> First c)
= First a
```

```haskell
  (First a <> First b) <> First c
= First a <> First c
= First a
```

## Monoid Law

### Case (1) (a = Nothing)

```haskell
  First a <> mempty
= First a

  mempty <> First a
= First Nothing <> First a
= First a
```

### Case (2) (a = Just a')

```haskell
  First a <> mempty
= First a

  mempty <> First a
= First Nothing <> First a
= First a
```

## なぜ getFirst :: a にしないのか？

普通は他のモノイド同様に以下のように定義したくなりますよね。

```haskell
newtype First a = First { getFirst :: a }

instance Semigroup (First a) where
  First a <> _ = First a

instance Monoid a => Monoid (First a) where
  mempty = First mempty
```

ではなぜこのような定義にしないのでしょうか？

答えは、`Monoid law` を満たさないからです。実際に確認してみましょう。

```haskell
  First a <> mempty
= First a

mempty <> First a
= First mempty <> First a
= First mempty
```

このように成り立ちません。ちなみに `Semigroup law` は満たします。

```haskell
  First a <> (First b <> First c)
= First a

  (First a <> First b) <> First c
= First a <> First c
= First a
```

つまり、Semigroup からモノイドにするために `Maybe` でラップしてあると考えて差し支えないと思います。

```haskell
-- Semigroup
newtype First a = First { getFirst :: a }

-- Semigroup, Monoid
newtype First a = First { getFirst :: Maybe a }
```

## 具体例: コマンドラインパーサー

僕が `First` モノイドの存在を認識したのは `stack` のコードで [ConfigMonoid](https://www.stackage.org/haddock/lts-13.6/stack-1.9.3/Stack-Types-Config.html#t:ConfigMonoid) というデータ型があり、その設定値に `First Bool` などの型が使われているという場面でした。

### コード

[optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) を使った具体的なサンプルはこんな感じです。(Last モノイドは名前の通りです)

```haskell
module Main (main) where

import Data.Monoid
import Options.Applicative

data PartialOptions = PartialOptions
  { poInputPath  :: First FilePath
  , poOutputPath :: Last  FilePath
  } deriving (Show, Eq)

partialOptionsParser :: Parser PartialOptions
partialOptionsParser = toPartialOptions
  <$> many (optFilePathP 'i')
  <*> many (optFilePathP 'o')
  where
    toPartialOptions input output =
      PartialOptions
        { poInputPath  = lift input
        , poOutputPath = lift output
        }

lift :: (Foldable t, Monoid (f a), Applicative f) => t a -> f a
lift = foldMap pure

optFilePathP :: Char -> Parser FilePath
optFilePathP = strOption . short

main :: IO ()
main = do
  options <- execParser $ info partialOptionsParser mempty 
  print options
```

### 実行結果

実行すると何が起きているかわかります。(手動で改行等を入れて見やすくしています)

```shell
# オプション無しで実行
λ> stack run ex2
PartialOptions
  { poInputPath  = First { getFirst = Nothing }
  , poOutputPath = Last  { getLast  = Nothing }
  }

# どちらもオプションを1つずつ指定して実行
λ> stack run ex2 -- -i aaa -o aaa
PartialOptions
  { poInputPath  = First { getFirst = Just "aaa" }
  , poOutputPath = Last  { getLast  = Just "aaa" }
  }

# -i のオプションのみ2つ指定
λ> stack run ex2 -- -i aaa -i bbb -o aaa
PartialOptions
  { poInputPath  = First { getFirst = Just "aaa" }
  , poOutputPath = Last  { getLast  = Just "aaa" }
  }

# -o のオプションのみ2つ指定
λ> stack run ex2 -- -i aaa -o aaa -o bbb
PartialOptions
  { poInputPath  = First { getFirst = Just "aaa" }
  , poOutputPath = Last  { getLast  = Just "bbb" }
  }

# -i, -o のオプションどちらも2つ指定
λ> stack run ex2 -- -i aaa -i bbb -o aaa -o bbb
PartialOptions
  { poInputPath  = First { getFirst = Just "aaa" }
  , poOutputPath = Last  { getLast  = Just "bbb" }
  }
```

こんな感じで `First` モノイドや `Last` モノイドを使ってオプションが複数指定された場合に最初の値か最後の値かを選ぶことができます。

## 参考

- [The Partial Options Monoid](https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67)
- [SemigroupがMonoidに恋するとき](https://kazu-yamamoto.hatenablog.jp/entry/2018/11/29/155311)
- [optparse-applicativeをふわっと使う](https://qiita.com/philopon/items/a29717af62831d3c8c07)