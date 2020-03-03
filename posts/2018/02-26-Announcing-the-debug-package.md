---
title: debug パッケージのアナウンス (翻訳)
author: Neil Mitchell
translator: pythonissam
tags: Neil Mitchell's Haskell Blog, 翻訳
updated: 2018/03/14
---

## debug パッケージのアナウンス (翻訳)

Original post: [Announcing the 'debug' package](http://neilmitchell.blogspot.jp/2017/12/announcing-debug-package.html)

Haskell は素晴らしい言語ですが、Haskell のデバッグは間違いなく弱い部分です。この問題をどうにかするために、[debug ライブラリ](https://hackage.haskell.org/package/debug) をリリースしました。このライブラリは全てのことを解決するのではなく、一般的なデバッグのタスクに対してシンプルで簡単に使えるような設計を目指しました。デバッグをするにあたって、みなさんが興味を持ってくれるような関数を例に取って見てみましょう。

<!--more-->

```haskell
module QuickSort(quicksort) where
import Data.List

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
    where (lt, gt) = partition (<= x) xs
```

`TemplateHaskell` と `ViewPatterns` を有効にしてから、`Debug` をインポートし、コードをインデントして `debug` の引数に渡してやりましょう。

```haskell
{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module QuickSort(quicksort) where
import Data.List
import Debug

debug [d|
   quicksort :: Ord a => [a] -> [a]
   quicksort [] = []
   quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort gt
       where (lt, gt) = partition (<= x) xs
   |]
```

以下のコマンドでデバッガを走らせることができます。

```shell
$ ghci QuickSort.hs
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling QuickSort        ( QuickSort.hs, interpreted )
Ok, 1 module loaded.
*QuickSort> quicksort "haskell"
"aehklls"
*QuickSort> debugView
```

`debugView` を呼ぶと Webブラウザが起動して、記録されたデバッグの結果を見ることができます。こんな感じです。

![debugView の実行結果](https://cdn.rawgit.com/ndmitchell/debug/f6e8dbc9/debug.png)

ここから、計算の経過をクリックしつつ探すことができます。

私は `debug` を使った感想や、これを改善できるようなアイディアを求めています。そのため、フィードバックや[バグトラッカー](https://github.com/ndmitchell/debug/issues)経由のお手伝いなどをお待ちしています。

また別の Haskell のデバッガに興味があるのなら、[GHCi デバッガ](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-debugger)や [Hood/Hoed](https://hackage.haskell.org/package/Hoed) などを試してみるべきでしょう。
