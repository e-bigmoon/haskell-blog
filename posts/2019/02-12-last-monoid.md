---
title: Last Monoid
author: Shinya Yamaguchi
tags: bigmoon, monoid
---

## はじめに

[Data.Monoid](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html) に [Last](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Last) 型が定義されています。

```haskell
newtype Last a = Last { getLast :: Maybe a }

instance Semigroup (Last a) where
  a <> Last Nothing = a
  _ <> b            = b

instance Monoid (Last a) where
  mempty = Last Nothing
```

`Last` モノイドは `First` モノイドとほとんど同じですが、`<>` で結合した時に最後の値を返すという部分が異なります。

使い方は簡単。

```haskell
ghci> getLast (Last (Just "hello") <> mempty <> Last (Just "world"))
Just "world"

ghci> getLast $ foldMap Last [Just "hello", Nothing, Just "world"]
Just "world"
```

<!--more-->

## Semigroup law の確認

### Case (1) (b == Nothing, c == Nothing)

```haskell
  Last a <> (Last b <> Last c)
= Last a <> Last b
= Last a
```

```haskell
  (Last a <> Last b) <> Last c
= Last a <> Last b
= Last a
```

### Case (2) (b == Just b', c == Nothing)

```haskell
  Last a <> (Last b <> Last c)
= Last a <> Last b
= Last b
```

```haskell
  (Last a <> Last b) <> Last c
= Last a <> Last b
= Last b
```

### Case (3) (c == Just c')

```haskell
  Last a <> (Last b <> Last c)
= Last a <> Last c
= Last c
```

```haskell
  (Last a <> Last b) <> Last c
= Last c
```

## Monoid Law

### Case (1) (a = Nothing)

```haskell
  Last a <> mempty
= Last a <> Last Nothing
= Last a

  mempty <> Last a
= mempty
= Last Nothing
```

### Case (2) (a = Just a')

```haskell
  Last a <> mempty
= Last a <> Last Nothing
= Last a

  mempty <> Last a
= Last a
```

## 具体例: Partial Options Monoid

コマンドライン引数によりオプションを受け取り、指定されなかったオプションの値に対してはデフォルト値を利用するという場面で `Last` モノイドが活用できそうです。

### コード

[optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) を使った具体的なサンプルはこんな感じです。

```haskell
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Monoid
import Options.Applicative

data Options = Options
  { oInputPath  :: FilePath
  , oOutputPath :: FilePath
  , oLogLevel   :: Maybe Int
  } deriving (Show, Eq)

data PartialOptions = PartialOptions
  { poInputPath  :: Last FilePath
  , poOutputPath :: Last FilePath
  , poLogLevel   :: Last (Maybe Int)
  } deriving (Show, Eq)

instance Semigroup PartialOptions where
  x <> y =
    PartialOptions
      { poInputPath  = poInputPath  x <> poInputPath  y
      , poOutputPath = poOutputPath x <> poOutputPath y
      , poLogLevel   = poLogLevel   x <> poLogLevel   y
      }

instance Monoid PartialOptions where
  mempty = PartialOptions mempty mempty mempty

defaultPartialOptions :: PartialOptions
defaultPartialOptions = mempty
  { poInputPath  = pure "input"
  , poLogLevel   = pure Nothing
  }

lastOption :: Parser a -> Parser (Last a)
lastOption = fmap Last . optional

partialOptionsParser :: Parser PartialOptions
partialOptionsParser = PartialOptions
  <$> lastOption (strOption (short 'i'))
  <*> lastOption (strOption (short 'o'))
  <*> lastOption (Just <$> option auto (short 'l'))

lastToEither :: String -> Last a -> Either String a
lastToEither errMsg = maybe (Left errMsg) Right . getLast

mkOptions :: PartialOptions -> Either String Options
mkOptions PartialOptions {..} = do
  oInputPath  <- lastToEither "Missing input path"  poInputPath
  oOutputPath <- lastToEither "Missing output path" poOutputPath
  oLogLevel   <- lastToEither "Missing loglevel"    poLogLevel
  return Options {..}

main :: IO ()
main = do
  options <- execParser $ info partialOptionsParser mempty
  case mkOptions (defaultPartialOptions <> options) of
    Left  msg -> putStrLn msg
    Right opt -> print opt
```

`defaultPartialOptions` でオプションの初期値を用意しておきます。ここで指定されなかったフィールドの値はオプションで必ず指定しなければなりません。今回の例では `poOutputPath` が必須オプションになっています。

また `Last` モノイドが効いている部分は `defaultPartialOptions <> options` です。`mempty = Last Nothing` となるため、期待通りの動作が得られます。

デフォルト値の無いオプションが省略された場合にエラーメッセージが表示される理由としては `lastToEither` で `getLast` した際に `Nothing` となるためです。

### 実行結果

実行結果は見やすく整形しています。

```shell
# オプション無しで実行
λ> stack run ex3
Missing output path

# 必須オプションの -o のみ指定 (他はデフォルト値)
$ stack run ex3 -- -o "oDir"
Options
  { oInputPath  = "input"
  , oOutputPath = "oDir"
  , oLogLevel   = Nothing
  }

# 必須オプションの -o と -i を指定
$ stack run ex3 -- -o "oDir" -i "myDir"
Options
  { oInputPath  = "myDir"
  , oOutputPath = "oDir"
  , oLogLevel   = Nothing
  }

# オプションを全部指定
$ stack run ex3 -- -o "oDir" -i "myDir" -l 10
Options
  { oInputPath  = "myDir"
  , oOutputPath = "oDir"
  , oLogLevel   = Just 10
  }
```

## 参考

- [The Partial Options Monoid](https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67)
- [SemigroupがMonoidに恋するとき](https://kazu-yamamoto.hatenablog.jp/entry/2018/11/29/155311)
- [optparse-applicativeをふわっと使う](https://qiita.com/philopon/items/a29717af62831d3c8c07)