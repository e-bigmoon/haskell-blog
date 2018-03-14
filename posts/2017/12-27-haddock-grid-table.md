---
title: haddock に Grid Table 記法が追加されました
author: Shinya Yamaguchi
tags: bigmoon, haddock
---

![Grid Table 記法の紹介](/images/2017/12-27/step0.png)

<!--more-->

## はじめに

みなさん `haddock` を使ってますか？

コメントを残す時に `|` や `^` を追加するだけで `haddock` 形式のコメントを残すことができます。

具体的にはこんな感じです。

```haskell
-- | リストの合計を計算する関数
sum :: Num a => [a] -> a
sum = foldl (+) 0

data Person
  { name :: Text -- ^ フルネーム
  }
```

コメントを `haddock` 形式にしてあげることで `HTML` のドキュメントがより充実するので、少ない労力で凄く楽しい気持ちになります。

`stack` を使っている場合はこのようにしてビルドするだけです。

```shell
$ stack haddock --open
```

今回、この `haddock` に `Grid Table` 記法が追加されたそうなので、その機能についてご紹介したいと思います。

## Grid Table 記法

以下の内容を参考としています。

- [Grid Tables #718](https://github.com/haskell/haddock/pull/718)
- [Add markup support for tables #530](https://github.com/haskell/haddock/issues/530)
- [Grid Tables](http://haskell-haddock.readthedocs.io/en/latest/markup.html#grid-tables)

### haddock の更新

`Grid Table` を使うためには `haddock-2.18.2` 以上である必要があります。

現状 `Hackage` の最新版が `2.18.1` なので `github` の最新版を利用する必要があります。

```shell
$ haddock --version
Haddock version 2.18.1, (c) Simon Marlow 2006
Ported to use the GHC API by David Waern 2006-2008
```

ここでは `stack` を使って最新版をインストールします。(その他の各種ビルド方法については [Readme](https://github.com/haskell/haddock#hacking) に詳しく載っているので、そちらをご参照ください)

```shell
$ git clone https://github.com/haskell/haddock.git
$ cd haddock
$ stack init
$ stack install

$ haddock --version
Haddock version 2.18.2, (c) Simon Marlow 2006
Ported to use the GHC API by David Waern 2006-2008

$ cd ../
```

これで準備は整いました！

### 使ってみる

まずは新規プロジェクトを作って、初期状態で `haddock` を生成します。

```shell
$ stack new test-haddock-grid-table
$ cd test-haddock-grid-table
$ stack haddock --open
```

ブラウザが自動的に立ち上がるので `Lib` モジュールを見てみましょう。

![初期状態で生成されるHTML](/images/2017/12-27/step1.png)

`someFunc` だけの味気ない `HTML` ですね。`haddock` コメントを追加して、もう一度確認してみます。

```haskell
-- src/Lib.hs
module Lib
    ( someFunc
    ) where

-- | haddock test
someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

```shell
$ stack haddock --open
```

![haddock コメントを少し追加](/images/2017/12-27/step2.png)

ちょっと変わりましたね。

では、本題の `Grid Table` 記法で書いてみます。

```haskell
-- src/Lib.hs
module Lib
    ( someFunc
    ) where

-- | Table with header.
--
-- +------+--------------+------------------------------------------+
-- | code | message      | description                              |
-- +======+==============+==========================================+
-- | 200  |   @OK@       | operation successful                     |
-- +------+--------------+------------------------------------------+
-- | 204  | @No Content@ | operation successful, no body returned   |
-- +------+--------------+------------------------------------------+
someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

ドキュメントを生成してみましょう。

```shell
$ stack haddock --open
```

![Grid Table の生成に失敗](/images/2017/12-27/step3.png)

失敗しました・・・。

これはどうやら `stack` が利用している `haddock` が `~/.local/bin` にインストールした最新版の `haddock` を参照していないためです。

- [Allow use of custom haddock binary to generate the documentation #2922](https://github.com/commercialhaskell/stack/issues/2922)
- [ow to use a custom haddock executable to generate documentation with stack?](https://stackoverflow.com/questions/41628606/how-to-use-a-custom-haddock-executable-to-generate-documentation-with-stack)

以下のコマンドで確認することができます。

```shell
$ stack exec -- which haddock
/home/bm12/.stack/programs/x86_64-linux/ghc-nopie-8.2.2/bin/haddock

$ stack exec -- haddock --version
Haddock version 2.18.1, (c) Simon Marlow 2006
Ported to use the GHC API by David Waern 2006-2008
```

ここで、`stack` が参照する `haddock` プログラムはシンボリックリンクとなっているため、一時的に上書きすることにしました。

```shell
$ stack path --compiler-bin
/home/bm12/.stack/programs/x86_64-linux/ghc-nopie-8.2.2/bin

$ ln -snf ~/.local/bin/haddock $(stack path --compiler-bin)/
```

これでやっと `Grid Table` が使えます！

```shell
$ stack clean
$ stack haddock --open
```

![Grid Table のレンダリング結果](/images/2017/12-27/step4.png)

なかなかオシャレな感じです。

## 後片付け

シンボリックリンクを元に戻しておきましょう。

```shell
$ ls -l $(stack path --compiler-bin)
合計 32
lrwxrwxrwx 1 bm12 bm12    9 11月 27 00:39 ghc -> ghc-8.2.2*
-rwxr-xr-x 1 bm12 bm12  444 11月 27 00:39 ghc-8.2.2*
lrwxrwxrwx 1 bm12 bm12   10 11月 27 00:39 ghci -> ghci-8.2.2*
-rwxr-xr-x 1 bm12 bm12  108 11月 27 00:39 ghci-8.2.2*
lrwxrwxrwx 1 bm12 bm12   13 11月 27 00:39 ghc-pkg -> ghc-pkg-8.2.2*
-rwxr-xr-x 1 bm12 bm12  476 11月 27 00:39 ghc-pkg-8.2.2*
lrwxrwxrwx 1 bm12 bm12   29 12月 27 14:24 haddock -> /home/bm12/.local/bin/haddock*
-rwxr-xr-x 1 bm12 bm12  435 11月 27 00:39 haddock-ghc-8.2.2*
-rwxr-xr-x 1 bm12 bm12  408 11月 27 00:39 hp2ps*
-rwxr-xr-x 1 bm12 bm12  406 11月 27 00:39 hpc*
-rwxr-xr-x 1 bm12 bm12 1206 11月 27 00:39 hsc2hs*
lrwxrwxrwx 1 bm12 bm12   12 11月 27 00:39 runghc -> runghc-8.2.2*
-rwxr-xr-x 1 bm12 bm12  452 11月 27 00:39 runghc-8.2.2*
lrwxrwxrwx 1 bm12 bm12    6 11月 27 00:39 runhaskell -> runghc*

$ ln -snf $(stack path --compiler-bin)/haddock-ghc-8.2.2 $(stack path --compiler-bin)/haddock
$ cd ../
$ rm -rf test-haddock-grid-table
$ rm -rf haddock
$ rm ~/.local/bin/haddock
```

## 終わりに

`stack` の謎の挙動によって若干苦戦しながらも、無事に試してみることができました。

`--with-haddock` のようなオプションが欲しいなぁと感じました・・・。

`Grid Table` についてはデータベース系の操作結果などを例示する際に使えそうです！

以上です。