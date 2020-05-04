---
title: LiquidHaskell のインストールと学習方法
author: Shinya Yamaguchi
tags: bigmoon, liquidhaskell
updated: 2020/05/02
---

## はじめに

**LiquidHaskell** を半年ほど勉強した結果、色々と出来ることが増えて楽しくなってきました。

現状、日本語で詳しく説明しているブログ記事等はほとんどありません。

とても面白いツールだと思いますので、色々と紹介していけたらと思います。

今回は **LiquidHaskell** の導入方法について簡単に説明したいと思います。

<!--more-->

## LiquidHaskell とは？

**LiquidHaskell** は **GHC** の型よりも、さらに厳密な **篩型 (Refinement Type)** の型検査器です。

既存のコードを変更 (さらには実行すら) することなく利用できるため、既存のプロジェクトの一部にだけ導入することも可能です。

また、つい最近も **GADT** をサポートしたりと、開発はとても活発に行われています。

正しいソフトウェアを楽しく作るために、**LiquidHaskell** を学習してみるのはどうでしょうか！

ちなみに **Liquid** という単語は **液体** を連想させますが、それとはあまり関係なく、実際は `Logically Qualified Data` の略です。(ロゴは **水滴** + **>>=** なので、全く無関係では無いかもですが)

## インストール

**LiquidHaskell** は以下の2つのリポジトリで開発が進められています。

- [ucsd-progsys/liquidhaskell](https://github.com/ucsd-progsys/liquidhaskell)
- [ucsd-progsys/liquid-fixpoint](https://github.com/ucsd-progsys/liquid-fixpoint)

`liquidhaskell` がフロントエンド (コマンドライン処理やパーサーなどの処理等) を行い `liquid-fixpoint` が **SMT** ソルバに投げるための処理を色々とやっている印象です。(詳しくないので間違ってたらすみません)

なので、僕らが関係するのは基本的に `liquidhaskell` リポジトリの方です。(`liquid-fixpoint` はサブモジュールになっています)

また、実際にチェックを行うのは **SMT** ソルバなので、そちらも同様にインストールが必要です。

### SMT ソルバのインストール

**SMT** ソルバも色々と種類があるようで、公式では以下の3種類が紹介されています。

- [Z3][z3-github]
- [CVC4][cvc4-github]
- [MathSat][mathsat-official]

[z3-github]: https://github.com/Z3Prover/z3
[cvc4-github]: https://github.com/CVC4/CVC4
[mathsat-official]: https://mathsat.fbk.eu/

どれでもちゃんと動くので好きなソルバを使えば良いのですが、どれを選んだら良いかわからない人は **Z3** にしましょう。

理由としてはインストール方法が簡単で、性能も良いそうです。

#### Ubuntu 18.04

```shell
$ sudo apt update
$ sudo apt install z3

$ z3 --version
Z3 version 4.4.1
```

#### macOS Catalina 10.15.4

**brew** でインストールする場合は以下の通りです。

```shell
$ brew install z3

$ z3 --version
Z3 version 4.8.7 - 64 bit
```

### LiquidHaskell のインストール

現状、一番安定しているのは **hackage** の [liquidhaskell][lh-hackage] を **cabal** と **GHC-8.6.5** の組み合わせでインストールする方法だと思います。その他のインストール方法等は [INSTALL.md][lh-install-doc] をご参照下さい。

```shell
$ ghc -V
The Glorious Glasgow Haskell Compilation System, version 8.6.5

$ cabal -V
cabal-install version 3.2.0.0
compiled using version 3.2.0.0 of the Cabal library
```

```shell
$ cabal update
$ cabal install liquidhaskell

$ liquid --version
LiquidHaskell Version 0.8.6.2 no git information
Copyright 2013-19 Regents of the University of California. All Rights Reserved.
```

**stack** プロジェクトで利用する場合は、以下のように `stack exec` コマンドで呼び出します。

```shell
$ stack exec -- liquid
LiquidHaskell Version 0.8.6.2 no git information
Copyright 2013-19 Regents of the University of California. All Rights Reserved.

Targets:
```

[lh-hackage]: https://hackage.haskell.org/package/liquidhaskell
[lh-install-doc]: https://github.com/ucsd-progsys/liquidhaskell/blob/develop/INSTALL.md

## サンプルプログラム

シンプルな `0` 除算の例を使って **LiquidHaskell** に慣れましょう！

```haskell
-- MyDiv.hs
module MyDiv where

myDiv :: Int -> Int -> Int
myDiv = div
```

`myDiv` の実装は単に `div` をラップしただけです。

この関数はだいたい上手く動きますが、もし第二引数に **0** が与えられたらどうでしょうか？そう、実行時エラーになります・・・。試してみましょう。

```shell
$ stack repl -- MyDiv.hs
*MyDiv> myDiv 10 2
5
*MyDiv> myDiv 10 0
*** Exception: divide by zero
```

全然安全ではありませんね。

では、どうしたら本当に安全な `myDiv` を作れるのでしょうか？

その答えは**篩(ふるい)型**にあります。

**LiquidHaskell** では **篩型** を `{-@ ... @-}` のコメント形式で記述します。**LiquidHaskell** を利用するメリットの1つは、篩型の自動推論です。(推論できない場合も多々ありますが、結構色々と推論してくれます)

先程の `myDiv` には篩型を書いていませんが、こういう場合に **LiquidHaskell** は **Haskell** の型をそのまま篩型として利用します。

`myDiv` に対して明示的に篩型を書いてみましょう！

```haskell
{-@ myDiv :: Int -> Int -> Int @-} -- これが篩型
myDiv :: Int -> Int -> Int
myDiv = div
```

この `myDiv` 関数を `LquidHaskell` でチェックしてみましょう。

```shell
$ liquid MyDiv.hs
**** RESULT: UNSAFE ************************************************************

MyDiv.hs:5:1-11: Error: Liquid Type Mismatch
  
 5 | myDiv = div
     ^^^^^^^^^^^
  
   Inferred type
     VV : GHC.Types.Int
  
   not a subtype of Required type
     VV : {VV : GHC.Types.Int | VV /= 0}
```

なぜか **UNSAFE** が表示されましたね。これは **LiquidHaskell** で既に `div` の篩型が定義されているからです。([`div` 以外にも色々あります](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/GHC/Real.spec#L19)が、充実しているとは言い難いと思います)

だいたいこんな感じで、第二引数に **0を含まないInt型** という事前条件がついているのです。

```haskell
{-@ div :: Int -> {v:Int | v /= 0} -> Int @-}
```

そのため、先程のエラーメッセージで以下のように指摘されてしまったのです。

```shell
not a subtype of Required type
     VV : {VV : GHC.Types.Int | VV /= 0}
```

つまり、僕らの定義した篩型は `0` を含む `Int` 型なので、このままだと `div` に `0` が与えられてしまう可能性があるよ！ということを教えてくれています。

```haskell
{-@ myDiv :: Int -> Int -> Int @-}
myDiv :: Int -> Int -> Int
myDiv = div
```

`myDiv` にも同じ篩型をつけてみましょう。

```haskell
{-@ myDiv :: Int -> {v:Int | v /= 0} -> Int @-}
myDiv :: Int -> Int -> Int
myDiv = div
```

これで **SAFE** になります。

```shell
$ liquid MyDiv.hs
**** RESULT: SAFE **************************************************************
```

`0` を含まない `Int` 型というのは、よく使いそうなので篩型のエイリアスとして定義してみます。

篩型のエイリアスは `type` キーワードを使います。**Haskell** と同じですね。

```haskell
{-@ type NonZero = {v:Int | v /= 0} @-}
```

そして `myDiv` の篩型も `NonZero` で置き換えます。

```haskell
{-@ type NonZero = {v:Int | v /= 0} @-}

{-@ myDiv :: Int -> NonZero -> Int @-}
myDiv :: Int -> Int -> Int
myDiv = div
```

意味は全く同じですが、先程よりもわかりやすくなりました。

```shell
$ liquid MyDiv.hs
**** RESULT: SAFE **************************************************************
```

最後に `myDiv` を呼び出す関数を定義してみましょう。

関数 `good` は問題の無い使い方です。

```haskell
good :: Int
good = myDiv 10 2
```

しかし、以下のような関数 `bad` が定義された場合、**LiquidHaskell** は **UNSAFE** を返します。

```haskell
bad :: Int
bad = myDiv 10 0
```

```shell
$ liquid MyDiv.hs
**** RESULT: UNSAFE ************************************************************

MyDiv.hs:12:16: Error: Liquid Type Mismatch
  
 12 | bad = myDiv 10 0
                     ^
  
   Inferred type
     VV : {v : GHC.Types.Int | v == 0}
  
   not a subtype of Required type
     VV : {VV : GHC.Types.Int | VV /= 0}
```

## 問題

以下のプログラムは標準入力から入力された数 `n`, `m` で `safeDiv n m` を計算します。

`safeDiv` の `check` を正しく実装して **LiquidHaskell** の結果を **SAFE** にしてみましょう。

```haskell
-- Main.hs
module Main where

{-@ type NonZero = {v:Int | v /= 0} @-}

{-@ myDiv :: Int -> NonZero -> Int @-}
myDiv :: Int -> Int -> Int
myDiv = div

{-@ lazy main @-}
main :: IO ()
main = do
  n <- getLine
  m <- getLine
  case safeDiv (read n) (read m) of
    Just res -> print res
    Nothing -> do
      putStrLn "第二引数に0が入力されています"
      putStrLn "もう一度入力してください"
      main

{-@ safeDiv :: Int -> Int -> Maybe Int @-}
safeDiv :: Int -> Int -> Maybe Int
safeDiv n m
  | check     = Just $ div n m
  | otherwise = Nothing
  where
   check = True
```

Hint: `div` に `0` を通さないよう `check` でバリデーションすれば良いです。

### エラーメッセージ

現状では、**LiquidHaskell** は以下のエラーメッセージを返します。

```shell
$ liquid Main.hs
**** RESULT: UNSAFE ************************************************************

Main.hs:24:30: Error: Liquid Type Mismatch
  
 24 |   | check     = Just $ div n m
                                   ^
  
   Inferred type
     VV : {v : GHC.Types.Int | v == m}
  
   not a subtype of Required type
     VV : {VV : GHC.Types.Int | VV /= 0}
  
   In Context
     m : GHC.Types.Int
```

### 実行例

**LiquidHaskell** が **UNSAFE** の場合は実行時エラーが発生します。

```shell
$ stack repl -- Main.hs
*Main> main
10
2
5

*Main> main
10
0
*** Exception: divide by zero
```

**LiquidHaskell** を **SAFE** にすると、再入力を促すようになります。

```shell
*Main> main
10
0
第二引数に0が入力されています
もう一度入力してください
10
2
5
```

## 興味を持った方へ

**LiquidHaskell** に興味を持った方は以下の文献を読んで **LiquidHaskell** に詳しくなりましょう！(個人的にまとめているやつを貼り付けただけなので雑ですみません・・・)

たぶん、おすすめは以下のチュートリアルです。(僕はまだ読んでないですが、かなり最近できたものなので情報が新しく良いのではないかと思います)

- [Liquid Haskell Tutorial](https://liquid.kosmikus.org/)

### 日本語の情報

- [LiquidHaskell コトハジメ](http://ccvanishing.hateblo.jp/entry/2016/12/24/193038)
- [入門 LiquidHaskell −篩型による静的コード解析−](https://dodgsonlabs.booth.pm/items/490689)
- [FLOPS2014報告](http://demand-side-science.jp/blog/2014/flops2014%E5%A0%B1%E5%91%8A/)

### ブログ

- [liquidhaskell-blog](https://ucsd-progsys.github.io/liquidhaskell-blog/)
- [Compile-time memory safety using Liquid Haskell](http://www.haskellforall.com/2015/12/compile-time-memory-safety-using-liquid.html)
- [Liquid Haskell: refinement types for the real world](http://conscientiousprogrammer.com/blog/2015/12/23/24-days-of-hackage-2015-day-23-liquid-haskell-refinement-types-for-the-real-world/)

### チュートリアル

- [Liquid Haskell Tutorial](https://liquid.kosmikus.org/)
- [liquidhaskell-tutorial](https://github.com/ucsd-progsys/liquidhaskell-tutorial)
  - long tutorial
  - [book](http://ucsd-progsys.github.io/liquidhaskell-tutorial/book.pdf)
  - [programming-with-refinement-types.pdf](https://github.com/ucsd-progsys/liquidhaskell-tutorial/blob/master/pdf/programming-with-refinement-types.pdf)
- [lh-workshop](https://github.com/ucsd-progsys/lh-workshop)
  - short tutorial
- [Liquid Haskell: Verification of Haskell Code](http://goto.ucsd.edu/~nvazou/presentations/shonan17/01-index.html)

### スライド

1. [Liquid Types For Haskell](http://goto.ucsd.edu/~rjhala/flops14/lhs/00_Index.lhs.slides.html#/)
1. [Simple Refinement Types](http://goto.ucsd.edu/~rjhala/flops14/lhs/01_SimpleRefinements.lhs.slides.html#/)
1. [Measuring Data Types](http://goto.ucsd.edu/~rjhala/flops14/lhs/02_Measures.lhs.slides.html#/)
1. [Higher-Order Specifications](http://goto.ucsd.edu/~rjhala/flops14/lhs/03_HigherOrderFunctions.lhs.slides.html#/)
1. [Abstract Refinements](http://goto.ucsd.edu/~rjhala/flops14/lhs/04_AbstractRefinements.lhs.slides.html#/)
1. [Lazy Evaluation?](http://goto.ucsd.edu/~rjhala/flops14/lhs/09_Laziness.lhs.slides.html#/)
1. [Refinements & Termination](http://goto.ucsd.edu/~rjhala/flops14/lhs/10_Termination.lhs.slides.html#/)
1. [Evaluation](http://goto.ucsd.edu/~rjhala/flops14/lhs/11_Evaluation.lhs.slides.html#/)

- [Finding and Fixing Bugs in Liquid Haskell](https://web.eecs.umich.edu/~weimerw/students/anish-ms-pres.pdf)
- [Scrap your Bounds Checks with Liquid Haskell](https://github.com/Gabriel439/slides/blob/master/liquidhaskell/slides.md)
- [Refinement Reflection: Complete Verification with SMT](https://nikivazou.github.io/static/popl18/reflection.pdf)
- [Liquid Haskell: Refinement Types for Haskell](https://popl18.sigplan.org/event/plmw-popl-2018-liquidhaskell-overview)

### 論文

- [Pat Rondon’s Ph.D Dissertation](http://goto.ucsd.edu/~pmr/papers/rondon-liquid-types.pdf)
- [Tech Report](http://goto.ucsd.edu/~rjhala/liquid/liquid_types_techrep.pdf)
- [Refinement Types For Haskell, ICFP 2014](http://goto.ucsd.edu/~rjhala/papers/refinement_types_for_haskell.pdf)
- [LiquidHaskell in the Real World, Haskell 2014](http://goto.ucsd.edu/~rjhala/papers/real_world_liquid.pdf)
- [Abstract Refinement Types, ESOP 2013](http://goto.ucsd.edu/~rjhala/papers/abstract_refinement_types.pdf)
- [An Introduction to Liquid Haskell](https://arxiv.org/pdf/1701.03320.pdf)
- [Liquid Haskell: Haskell as a Theorem Prover](http://goto.ucsd.edu/~nvazou/thesis/main.pdf)
- [A Tale of Two Provers Verifying Monoidal String Matching in Liquid Haskell and Coq](https://nikivazou.github.io/static/Haskell17/a-tale.pdf)

### YouTube

- [Liquid Types for Haskell](https://www.youtube.com/watch?v=LEsEME7JwEE)
- [Ranjit Jhala - Liquid Haskell](https://www.youtube.com/watch?v=vYh27zz9530)
- [LambdaConf 2015 - LiquidHaskell Refinement Types for Haskell Ranjit Jhala](https://www.youtube.com/watch?v=vQrutfPAERQ)
- [Haskell 2014: LiquidHaskell: Refinement Types for the Real World, Eric L. Seidel](https://www.youtube.com/watch?v=vqvNQixKr6w)
- [07 Bounded Refinement Types](https://www.youtube.com/watch?v=nd3buP97Ryw)

## まとめ

- 篩型は `{-@ ... @-}` で記述する
- 篩型の型エイリアスは `{-@ type @-}` で記述する

以上です。

## 本記事で利用したコード

- [MyDiv.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/2018/03-03/MyDiv.hs)
- [Main.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/2018/03-03/Main.hs)