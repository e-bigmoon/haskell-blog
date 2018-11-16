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

実行結果

```shell
ghci> ansSplit "CamelCase"
"Camel Case"
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

実行結果

```shell
ghci> ansFold "CamelCase"
"Camel Case"
```

## QuickCheck

念の為 [QuickCheck](https://www.stackage.org/package/QuickCheck) を使ってランダムテストを行ってみましょう。

```hs
module Main (main) where

import Test.QuickCheck

import SplitCC

newtype MyString = MyString { getString :: String }
  deriving (Eq, Show)

instance Arbitrary MyString where
  arbitrary = fmap MyString $ listOf $ elements (['a'..'z']++['A'..'Z'])

main :: IO ()
main = quickCheck prop_split

prop_split :: MyString -> Bool
prop_split xs = ansSplit xs' == ansFold xs'
  where xs' = getString xs
```

実行結果

```shell
$ stack test
splitcc-0.1.0.0: test (suite: splitcc-test)

+++ OK, passed 100 tests.

splitcc-0.1.0.0: Test suite splitcc-test passed
```

## ベンチマーク

次は [gauge](https://github.com/vincenthz/hs-gauge) を使ってどっちが速いか確認してみましょう。

```hs
import Gauge.Main
import Gauge.Main.Options

import Test.QuickCheck

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

## AutoBench

最後に [AutoBench](https://github.com/mathandley/AutoBench) を使って視覚的に実行時間の変化を確認してみましょう。

```hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Input (ts, ansSplit, ansFold) where

import           Data.Char          (isSpace, isUpper)
import           Data.List.Split    (split, startsWithOneOf)

import           GHC.Generics    (Generic)
import           Control.DeepSeq

import           Data.Default         (def)
import           AutoBench.Types      (DataOpts(..), TestSuite(..))
import           AutoBench.QuickCheck ()
import           Test.QuickCheck

ansSplit :: MyString -> String
ansSplit = unwords . split (startsWithOneOf ['A'..'Z']) . getString

ansFold :: MyString -> String
ansFold = fmt . foldr go [] . getString
  where
    go c acc
      | isUpper c = ' ':c:acc
      | otherwise = c:acc
    fmt cs
      | null cs = cs
      | isSpace (head cs) = tail cs
      | otherwise = cs

ts :: TestSuite
ts  = def { _dataOpts = Gen 0 10000 200000 }

newtype MyString = MyString { getString :: String }
  deriving (Eq, Show, Generic, NFData)

instance Arbitrary MyString where
  arbitrary = fmap MyString $ listOf $ elements (['a'..'z']++['A'..'Z'])
```

- AutoBench を利用する際、入力の型は `NFData` 型クラスのインスタンスになっている必要があります。
- デフォルトの設定だと実行完了までに5分程度かかります。

AutoBench の結果

![AutoBench の結果](/images/2018/11-16/AutoBenched.png)

一応ターミナルにもこんな感じで結果を出力してくれます。

```shell
 ―― Test summary ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

  Programs       ansFold, ansSplit
  Data           Random, size range [0,10000..200000]
  Normalisation  nf
  QuickCheck     ✔
  GHC flags      n/a

 ―― Analysis ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

  ansFold
    Size          0       10000   20000   30000   40000   50000   60000   70000
                  80000   90000   100000  110000  120000  130000  140000  150000
                  160000  170000  180000  190000  200000
    Time    (ms)  0.000   0.581   0.376   0.115   2.795   3.645   5.881   1.399
                  2.619   3.767   4.792   1.419   1.709   11.00   8.169   5.921
                  15.06   12.73   17.61   12.57   9.051
    Std dev (ms)  2.218
    Average variance introduced by outliers: 91% (severely inflated)

    Fits          y = 4.48e-13 + 6.12e-8x
                  y = 1.54e-23 + 8.69e-17x + 3.87e-13x²
                  y = 1.50e-15 + 3.57e-9xlog₂(x)

  ansSplit
    Size          0       10000   20000   30000   40000   50000   60000   70000
                  80000   90000   100000  110000  120000  130000  140000  150000
                  160000  170000  180000  190000  200000
    Time    (ms)  0.000   4.140   2.812   1.240   10.43   45.66   61.93   12.14
                  20.63   31.00   41.17   26.11   10.80   98.83   73.31   44.92
                  55.60   50.67   140.7   68.02   112.4
    Std dev (ms)  27.40
    Average variance introduced by outliers: 74% (severely inflated)

    Fits          y = 3.27e-12 + 4.47e-7x
                  y = 1.09e-14 + 2.61e-8xlog₂(x)
                  y = 1.11e-22 + 1.38e-16x + 2.79e-12x²

  Optimisation:

    ansSplit ≥ ansFold (1.00)

 ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
```

`ansSplit ≥ ansFold (1.00)` ということなので `ansFold` の方が良い結果となりました。

## まとめ

- リストを何度も走査すると遅くなるので、fold で書くと良いよ！
- 関数の振る舞いが変化していないか確認するために QuickCheck を使おう
- ベンチマークの実行はとても簡単なので積極的にやってみよう！
- AutoBench を使って可視化しよう！