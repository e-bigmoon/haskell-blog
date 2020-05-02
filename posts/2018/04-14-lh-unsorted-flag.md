---
title: LiquidHaskell の --prune-unsorted フラグ
author: Shinya Yamaguchi
tags: bigmoon, liquidhaskell
updated: 2020/05/02
---

## はじめに

**LiquidHaskell** では **measure** という仕組みを使って **Haskell** の関数を **LiquidHaskell** の述語に持ち上げることができます。

しかし、以下の `nLen` 関数は **measure** によって持ち上げることができません。

```hs
{-@ measure nLen @-}
nLen :: [Int] -> Int
nLen [] = 0
nLen (n:ns) = n + nLen ns
```

結論から言えば、これを解決するためには `--prune-unsorted` フラグを利用します。(**Version 0.8.6.2** ではフラグを有効にしなくても持ち上げることができました)

以下は **measure** の基本的な使い方等についての説明です。

<!--more-->

## measure の基本的な使い方

例えばリストの長さを求める `lLen` という関数を述語に持ち上げるにはこのようにします。

```hs
{-@ measure lLen @-}
lLen :: [a] -> Int
lLen [] = 0
lLen (_:xs) = 1 + lLen xs
```

```shell
$ liquid LH.hs 
LiquidHaskell Version 0.8.6.2 no git information
Copyright 2013-19 Regents of the University of California. All Rights Reserved.

Targets: LH.hs

**** [Checking: LH.hs] *********************************************************
**** DONE:  A-Normalization ****************************************************
**** DONE:  Extracted Core using GHC *******************************************
**** DONE:  Transformed Core ***************************************************
Working 100% [=================================================================]
**** DONE:  annotate ***********************************************************
**** RESULT: SAFE **************************************************************
```

実際にはこんな感じで長さ付きリストのリファインメント型をつけます。先程から**述語**と言っているのはリファインメント型 `{ | }` の `|` の右側のことです。**measure** によってこの部分で **Haskell** で定義した `lLen` 関数が使えるようになります。

```hs
{-@ type ListN a N = {v:[a] | lLen v = N} @-}

{-@ goodList :: ListN Int 2 @-}
goodList :: [Int]
goodList = [1,2]
```

当然ですが、仕様を満たしていない場合は `UNSAFE` になります。

```hs
{-@ badList :: ListN Int 1 @-}
badList :: [Int]
badList = [1,2]
```

```shell
**** RESULT: UNSAFE ************************************************************
 LH.hs:16:1-15: Error: Liquid Type Mismatch
  
 16 | badList = [1,2]
      ^^^^^^^^^^^^^^^
  
   Inferred type
     VV : {v : [GHC.Types.Int] | tail v == ?f
                                 && head v == 1
                                 && LH.lLen v == 1 + LH.lLen ?f
                                 && len v == 1 + len ?f
                                 && len v >= 0}
  
   not a subtype of Required type
     VV : {VV : [GHC.Types.Int] | LH.lLen VV == 1}
  
   In Context
     ?f : {?f : [GHC.Types.Int] | tail ?f == ?d
                                  && head ?f == 2
                                  && LH.lLen ?f == 1 + LH.lLen ?d
                                  && len ?f == 1 + len ?d
                                  && len ?f >= 0}
      
     ?d : {?d : [GHC.Types.Int] | LH.lLen ?d == 0
                                  && len ?d == 0
                                  && len ?d >= 0}
```

エラーメッセージがつらいですが、とりあえずは以下の部分に着目すれば良いでしょう。

```hs
not a subtype of Required type
     VV : {VV : [Int] | Main.lLen VV == 1}
```

`lLen VV == 1` の制約が満たされていない。つまり、`lLen` の結果が `1` になってないよ。ということです。

## measure でエラーが出る場合

**measure** はどんな関数にも使えるわけではありません。

例えば、以下のような数値のリストに対して、リストの値の合計を返す関数 `nLen` を考えましょう。(`sum` と同じですが `Prelude` とかぶるので名前を変更しています)

```hs
{-@ measure nLen @-}
nLen :: [Int] -> Int
nLen [] = 0
nLen (n:ns) = n + nLen ns
```

`LiquidHaskell` はこの定義に対してエラーを表示します。これは `nLen` 関数がリストの型変数 `a` を `Int` に特殊化しているためです。([Prune Unsorted Predicates](https://github.com/ucsd-progsys/liquidhaskell#prune-unsorted-predicates))

こういう関数に対しては `--prune-unsorted` フラグを用いれば良いです。

```hs
{-@ LIQUID "--prune-unsorted" @-}
{-@ measure nLen @-}
nLen :: [Int] -> Int
nLen [] = 0
nLen (n:ns) = n + nLen ns
```

`nLen` を使った型は、先ほどの `lLen` とほとんど同じです。

```hs
{-@ type ListSum a N = {v:[a] | nLen v = N} @-}

{-@ goodListSum :: ListSum Int 111 @-}
goodListSum :: [Int]
goodListSum = [1,10,100]

{-@ badListSum :: ListSum Int 111 @-}
badListSum :: [Int]
badListSum = [1]
```

## まとめ

- 型パラメータが特殊化された関数を **measure** で述語に持ち上げる際には `--prune-unsorted` を使う

以上です。

## 本記事で利用したコード

- [LH.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/2018/04-14/LH.hs)