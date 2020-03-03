---
title: Haddock コメント形式
published: 2017/12/24
updated: 2019/09/14
prev: ./haddock-comment.html
next: ./haddock-settings.html
---

詳しい書式については以下のドキュメントを適宜参照してください。

- [Documentation and Markup](http://haskell-haddock.readthedocs.io/en/latest/markup.html)

## 最低限覚えておく形式

**Haddock** 形式のコメントは非常に簡単です。

```haskell
-- 通常のコメント
f = undefined

-- | Haddock 形式のコメント
--   直後の関数についてのコメント
g = undefined
-- ^ この形式も同様に Haddock 形式のコメント
--   直前の関数についてのコメント
```

つまり、`-- |` か `-- ^` で始まるコメントが **Haddock** コメントということになります。

慣習的に関数のコメントには `-- |` を使い、型のフィールドについては `-- ^` を使っているように思います。

## もっとリッチな Haddock コメント

以下は公式のドキュメントに載っている例をまとめたものです。

これだけ知っていれば **Haddock** が書けると言って良いでしょう。強調のためのキーワードやモジュールのコメントなどはここでは紹介しないのでドキュメントをご参照ください。

```haskell
{-|
Module      : W
Description : Short description
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module W where

-- |The 'square' function squares an integer.
-- @since 1.0.0
square :: Int -> Int
square x = x * x

data T a b
  = C1 a b  -- ^ This is the documentation for the 'C1' constructor
  | C2 a b  -- ^ This is the documentation for the 'C2' constructor

data R a b =
  C { a :: a  -- ^ This is the documentation for the 'a' field
    , b :: b  -- ^ This is the documentation for the 'b' field
    }

f  :: Int      -- ^ The 'Int' argument
   -> Float    -- ^ The 'Float' argument
   -> IO ()    -- ^ The return value
f = undefined
```

**@since** メタデータは **Yesod** 関連のプロジェクトでは良く使われています。どのバージョンからこの関数が組み込まれたかをドキュメントに残すことができます。

## 実際に使ってみよう

練習として **minfree** 関数に **Haddock** コメントをつけてみましょう。

```haskell
-- |
-- 与えられた自然数のリストに含まれない最小の自然数を求める関数
-- 自然数は0を含む
-- 前提条件1: 与えられたリストには順序がついていない
-- 前提条件2: 要素は重複していない
minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)
```

まぁまぁ良いでしょう！ではドキュメントを生成してみます。

```shell-session
$ stack haddock --haddock-arguments --odir=haddock
$ firefox haddock/Minfree.html
```

![](/images/haddock03.jpeg)

失敗しました。流石に1行はちょっと読みづらいので、修正します。

修正は簡単で、改行したい場所に空行を挟むだけです。

```haskell
-- |
-- 与えられた自然数のリストに含まれない最小の自然数を求める関数
--
-- 自然数は0を含む
--
-- 前提条件1: 与えられたリストには順序がついていない
--
-- 前提条件2: 要素は重複していない
minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)
```

![](/images/haddock04.jpeg)

それっぽくなりましたね！
