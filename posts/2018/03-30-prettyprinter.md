---
title: prettyprinter パッケージ
author: Shinya Yamaguchi
tags: bigmoon, package
---

## はじめに

今回は [prettyprinter](https://www.stackage.org/package/prettyprinter) を使ってみました。

使おうと思ったモチベーションとしては、以下のようなデータ型をデバッグ表示させる際に `prettyprinter` 系パッケージを使って良い感じに表示してみようかな？という感じです。

```hs
data Person = Person Text String
```

表示をカスタマイズするために `Show` クラスのインスタンスを書き換えることは、経験上嫌なことしか起きないです・・。

自分でそれっぽい型クラスを作っても良いのですが、普通は素直に `prettyprinter` 系のパッケージを使った方が良いと思います！

```sh
$ stack repl --package text
Prelude> import Data.Text
Prelude Data.Text> data Person = Person Text String deriving Show
Prelude Data.Text> :set -XOverloadedStrings
Prelude Data.Text> Person "はすける" "らむだ"
Person "\12399\12377\12369\12427" "\12425\12416\12384"
```

<!--more-->

## パッケージの選定理由

ぐぐって一番始めに出てきたのは [pretty](https://github.com/haskell/pretty) というパッケージだったんですが `String` にしか対応してない感じだったので見送りました。

`prettyprinter` は **A modern, easy to use, well-documented, extensible prettyprinter.** というコメント通り、とても使いやすいです。

## 使ってみました

```hs
#!/usr/bin/env stack
-- stack --resolver lts-11.2 script
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Text (Text)

data A = A Text String
  deriving Show

data B = B Text String

instance Pretty B where
  pretty (B l r) = "B" <+> pretty l <+> pretty r

main :: IO ()
main = do
  print $ A "あ" "a"
  putDoc $ pretty $ B "あ" "a"
  putStrLn ""
```

```sh
$ chmod u+x Pretty.hs
$ ./Pretty.hs
A "\12354" "a"
B あ a
```

[Pretty](https://www.stackage.org/haddock/lts-11.2/prettyprinter-1.2.0.1/Data-Text-Prettyprint-Doc.html#t:Pretty) 型クラスのインスタンスを普通に定義するだけです。

適当に改行したい場合は [line](https://www.stackage.org/haddock/lts-11.2/prettyprinter-1.2.0.1/Data-Text-Prettyprint-Doc.html#v:line) を追加するだけです。

```hs
instance Pretty B where
  pretty (B l r) = "B" <> line
                <+> pretty l <> line
                <+> pretty r
```

```sh
$ ./test.hs
A "\12354" "a"
B
 あ
 a
```

先頭の空白が不必要な場合は [<+>](https://www.stackage.org/haddock/lts-11.2/prettyprinter-1.2.0.1/Data-Text-Prettyprint-Doc.html#v:-60--43--62-) を [<>](https://www.stackage.org/haddock/lts-11.2/prettyprinter-1.2.0.1/Data-Text-Prettyprint-Doc.html#v:-60--62-) にするだけです。

```hs
instance Pretty B where
  pretty (B l r) = "B" <> line
                <> pretty l <> line
                <> pretty r
```

```sh
$ ./test.hs
A "\12354" "a"
B
あ
a
```

適当にネストさせたい場合も簡単です。

```hs
instance Pretty B where
  pretty (B l r) = nest 4 ("B" <> line
                <> pretty l <> line
                <> pretty r)
```

```sh
$ ./test.hs
A "\12354" "a"
B
    あ
    a
```

## まとめ

めちゃめちゃ簡単だったので、使ってみると楽しいですよー。ドキュメントが充実しているのが良いですね。

今回は短めでした。

以上です。