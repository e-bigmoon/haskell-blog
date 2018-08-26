---
title: RecordWildCards と Reader モナド
author: Shinya Yamaguchi
tags: bigmoon
---

## はじめに

`Twitter` で `RecordWildCards` の話が流れて来たので `Reader` モナドと組み合わせた例を1つご紹介します。

この書き方は @fumieval さんが使っていてカッコイイ書き方だなぁと思った記憶があります。

```haskell
{-# LANGUAGE RecordWildCards #-}

data Person = Person
  { personName :: String
  , personAge  :: Int
  }

func :: Reader Person ()
func = ask >>= \Person {..} -> $ do
  ... -- personName, personAge という名前でそのまま値を利用できる
```

`RecordWildCards` に関しては既に色んな方が記事にしてくれているので、そちらをご参照ください。

- [GHC拡張ノック(Part 1)](https://haskell.jp/blog/posts/2018/about-ghc-exts-1.html)
- [Haskellの言語拡張たち 2](http://rf0444.hatenablog.jp/entry/20120617/1339910411)
- [波打たせるものの正体(エクステンシブル・タングル)](http://fumieval.hatenablog.com/entry/2016/12/18/181540)
- [GHC user guide 10.5.5. Record wildcards](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#record-wildcards)

<!--more-->

## 具体例

あまり良い例が思いつかなかったのですが、 `Env` に必要な情報を持たせておけば任意のタイミングで利用できて便利です。

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.7
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import           Data.Time.LocalTime
import           Path

data Env = Env
  { envCounter  :: IORef Int
  , envLogPath  :: Path Rel File
  , envUserName :: String
  }

main :: IO ()
main = do
  counter <- newIORef 0
  let env = Env
        { envCounter  = counter
        , envLogPath  = $(mkRelFile "./output.log")
        , envUserName = "wado"
        }

  runReaderT (access >> access >> access) env

  n <- readIORef counter
  putStrLn $ mconcat ["counter = ", show n]

access :: ReaderT Env IO ()
access = ask >>= \Env {..} -> liftIO $ do
  modifyIORef envCounter (+ 1)
  now <- getZonedTime
  appendFile (toFilePath envLogPath)
    $ mconcat [envUserName, ": ", "access", " (", show now, ")", "\n"]
  return ()
```

### 実行結果

```shell
$ ./Sample.hs
counter = 3

$ cat ./output.log
wado: access (2018-08-26 14:04:07.132813451 JST)
wado: access (2018-08-26 14:04:07.133330363 JST)
wado: access (2018-08-26 14:04:07.133389916 JST)
```

## missing-fields 警告

```hs
{-# LANGUAGE RecordWildCards #-}

data Person = Person
  { personName :: String
  , personAge  :: Int
  } deriving Show

f :: Person
f = Person { .. }
  where
    personName = "bigmoon"
```

上記のようにフィールドが全て初期化されていないコードはコンパイル時に `missing-fields` 警告が出ます。これは、実行時エラーになるかもしれないということを示唆しています。

無視せずにしっかり修正しましょう。

```hs
warning: [-Wmissing-fields]
    • Fields of ‘Person’ not initialised: personAge
    • In the expression: Person {..}
      In an equation for ‘f’:
          f = Person {..}
            where
                personName = "bigmoon"
   |
46 | f = Person { .. }
   |
```

ちなみに、通常のレコード構文でもフィールドが部分的にしか初期化されていない場合は、同様の警告がでます。(@fumieval さん、ご指摘ありがとうございます)

```haskell
data Person = Person
  { personName :: String
  , personAge  :: Int
  } deriving Show

f :: Person
f = Person { personName = "bigmoon" }
```

- [record wildcards: field not initialised reported as type error](https://ghc.haskell.org/trac/ghc/ticket/5334)

## まとめ

- RecordWildCards 言語拡張は現実世界ではとても便利 (レコードのフィールドがかなり沢山あったりするので)
- Reader とかと組み合わせるとオシャレ
- GHC の `missing-fields` 警告は絶対に無視しない (実行時エラーが発生する可能性がある)

以上です。
