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

splitCC :: String -> String
splitCC  = unwords . split (startsWithOneOf ['A'..'Z'])
```

実行結果

```shell
ghci> splitCC "CamelCase"
"Camel Case"
```

## fold

`split` パッケージを使った実装は直感的でコードもコンパクトです。

しかし、見た感じ効率悪そうですよね。(リスト全体を分割した後で `unwords` かけるため)

そのため、`fold` バージョンも実装してみましょう。

```hs
import Data.Char (isUpper, isSpace)

foldSplitCC :: String -> String
foldSplitCC = fmt . foldr go []
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
ghci> foldSplitCC "CamelCase"
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
prop_split xs = splitCC xs' == foldSplitCC xs'
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

import SplitCC

main :: IO ()
main = do
  let conf = defaultConfig { displayMode = Condensed }
  sampleData1 <- generate $ vectorOf 10 charGen
  sampleData2 <- generate $ vectorOf 1000 charGen
  sampleData3 <- generate $ vectorOf 100000 charGen
  sampleData4 <- generate $ vectorOf 1000000 charGen

  defaultMainWith conf
    [ bgroup "splitCC" [ bench "10"      $ nf splitCC sampleData1
                       , bench "1000"    $ nf splitCC sampleData2
                       , bench "100000"  $ nf splitCC sampleData3
                       , bench "1000000" $ nf splitCC sampleData4
                       ]
    , bgroup "foldSplitCC" [ bench "10"      $ nf foldSplitCC sampleData1
                           , bench "1000"    $ nf foldSplitCC sampleData2
                           , bench "100000"  $ nf foldSplitCC sampleData3
                           , bench "1000000" $ nf foldSplitCC sampleData4
                           ]
    ]

charGen :: Gen Char
charGen = elements (['a'..'z']++['A'..'Z'])
```

実行結果

```shell
$ stack bench
Benchmark splitcc-bench: RUNNING...
splitCC/10                               mean 2.847 μs  ( +- 261.0 ns  )
splitCC/1000                             mean 287.2 μs  ( +- 3.935 μs  )
splitCC/100000                           mean 29.10 ms  ( +- 1.348 ms  )
splitCC/1000000                          mean 287.3 ms  ( +- 735.0 μs  )

foldSplitCC/10                           mean 219.3 ns  ( +- 2.955 ns  )
foldSplitCC/1000                         mean 25.65 μs  ( +- 246.5 ns  )
foldSplitCC/100000                       mean 2.640 ms  ( +- 22.98 μs  )
foldSplitCC/1000000                      mean 27.43 ms  ( +- 1.795 ms  )
Benchmark splitcc-bench: FINISH
```

## AutoBench

最後に [AutoBench](https://github.com/mathandley/AutoBench) を使って視覚的に実行時間の変化を確認してみましょう。

```hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Input (ts, splitCC, foldSplitCC) where

import Data.Char          (isSpace, isUpper)
import Data.List.Split    (split, startsWithOneOf)

import GHC.Generics    (Generic)
import Control.DeepSeq

import Data.Default         (def)
import AutoBench.Types      (DataOpts(..), TestSuite(..))
import AutoBench.QuickCheck ()
import Test.QuickCheck

splitCC :: MyString -> String
splitCC = unwords . split (startsWithOneOf ['A'..'Z']) . getString

foldSplitCC :: MyString -> String
foldSplitCC = fmt . foldr go [] . getString
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

AutoBench の結果

![AutoBench の結果](/images/2018/11-16/AutoBenched.png)

一応ターミナルにもこんな感じで詳細な結果も出力してくれます。

```shell
     • Executed benchmarking file ✔
     • Generating test report ✔
     • Analysing performance results...

 ―― Test summary ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

  Programs       foldSplitCC, splitCC
  Data           Random, size range [0,10000..200000]
  Normalisation  nf
  QuickCheck     ✔
  GHC flags      n/a

 ―― Analysis ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

  foldSplitCC
    Size          0       10000   20000   30000   40000   50000   60000   70000
                  80000   90000   100000  110000  120000  130000  140000  150000
                  160000  170000  180000  190000  200000
    Time    (ms)  0.000   0.179   1.225   1.978   2.112   6.172   4.232   1.248
                  0.217   5.961   5.184   2.715   5.186   2.690   6.810   4.810
                  7.073   7.760   5.122   8.280   18.66
    Std dev (ms)  1.742
    Average variance introduced by outliers: 83% (severely inflated)

    Fits          y = 1.18e-23 + 8.49e-17x + 2.98e-13x²
                  y = 3.45e-13 + 4.71e-8x
                  y = 3.47e-34 + 5.68e-29x + 9.70e-24x² + 1.70e-18x³

  splitCC
    Size          0       10000   20000   30000   40000   50000   60000   70000
                  80000   90000   100000  110000  120000  130000  140000  150000
                  160000  170000  180000  190000  200000
    Time    (ms)  0.000   1.080   9.397   13.53   6.133   39.55   29.14   9.504
                  1.596   44.46   38.58   16.35   35.41   17.94   38.40   52.06
                  59.35   62.88   40.68   58.27   105.0
    Std dev (ms)  10.54
    Average variance introduced by outliers: 62% (severely inflated)

    Fits          y = 2.41e-12 + 3.29e-7x
                  y = 8.06e-15 + 1.92e-8xlog₂(x)
                  y = 8.20e-23 + 1.21e-16x + 2.07e-12x²

  Optimisation:

    splitCC ≥ foldSplitCC (1.00)

 ――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
```

`splitCC ≥ foldSplitCC (1.00)` ということなので `foldSplitCC` の方が良い結果となりました。

## まとめ

- リストを何度も走査すると遅くなるので、fold で書くと良いよ！
- 関数の振る舞いが変化していないか確認するために QuickCheck を使おう！
- ベンチマークの実行はとても簡単なので積極的にやってみよう！
- AutoBench を使って可視化すると楽しいよ！