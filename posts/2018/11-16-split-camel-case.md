---
title: CamelCase の split
author: Shinya Yamaguchi
tags: bigmoon
---

## はじめに

Twitter で `CamelCase` の文字列を `Camel Case` にするという話を見かけたので、やってみました。

文字列に含まれる文字は `['a'..'z'] ++ ['A'..'Z']` を想定しています。

<!--more-->

## split

分割するということなので [split](https://www.stackage.org/package/split) パッケージを使ってみます。

```hs
import Data.List.Split (split, startsWithOneOf)

ansSplit :: String -> String
ansSplit = unwords . split (startsWithOneOf ['A'..'Z'])
```

## fold

`split` パッケージを使った実装は直感的でコードもコンパクトです。

しかし、見た感じ効率悪そうですよね。(リスト全体を分割した後で `unwords` かけるため)

そのため、`fold` バージョンも実装してみましょう。

```hs
import Data.Char (isUpper, isSpace)

ansFold :: String -> String
ansFold = fmt . foldr go []
  where
    go c acc
      | isUpper c = ' ':c:acc
      | otherwise = c:acc
    fmt cs
      | null cs = cs
      | isSpace (head cs) = tail cs
      | otherwise = cs
```

## ベンチマーク

- [gauge](https://github.com/vincenthz/hs-gauge) を使ってどっちが速いか確認してみましょう。

```hs
import Gauge.Main
import Gauge.Main.Options

import Test.QuickCheck

-- bench
main :: IO ()
main = do
  let conf = defaultConfig { displayMode = Condensed }
  sampleData1 <- generate $ vectorOf 10 charGen
  sampleData2 <- generate $ vectorOf 1000 charGen
  sampleData3 <- generate $ vectorOf 100000 charGen
  sampleData4 <- generate $ vectorOf 10000000 charGen

  defaultMainWith conf
    [ bgroup "ansSplit" [ bench "10" $ whnf ansSplit sampleData1
                        , bench "1000" $ whnf ansSplit sampleData2
                        , bench "100000" $ whnf ansSplit sampleData3
                        , bench "10000000" $ whnf ansSplit sampleData4
                        ]
    , bgroup "ansFold"  [ bench "10" $ whnf ansFold sampleData1
                        , bench "1000" $ whnf ansFold sampleData2
                        , bench "100000" $ whnf ansFold sampleData3
                        , bench "10000000" $ whnf ansFold sampleData4
                        ]
    ]

charGen :: Gen Char
charGen = elements (['a'..'z']++['A'..'Z'])
```

実行結果

```shell
$ stack bench
Benchmark splitcc: RUNNING...
ansSplit/10                              mean 538.8 ns  ( +- 274.7 ns  )
ansSplit/1000                            mean 423.3 ns  ( +- 65.61 ns  )
ansSplit/100000                          mean 343.1 ns  ( +- 35.88 ns  )
ansSplit/10000000                        mean 725.5 ns  ( +- 132.6 ns  )
ansFold/10                               mean 26.40 ns  ( +- 8.706 ns  )
ansFold/1000                             mean 25.17 ns  ( +- 2.550 ns  )
ansFold/100000                           mean 21.20 ns  ( +- 2.566 ns  )
ansFold/10000000                         mean 25.27 ns  ( +- 2.758 ns  )
Benchmark splitcc: FINISH
```

## まとめ

- ベンチマークの実行はとても簡単なので積極的にやってみよう！
- リストを何度も走査すると遅くなるので、fold で書くと良いよ！