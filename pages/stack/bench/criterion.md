---
title: criterion パッケージ
date: 2019/09/15
prev: ./index.html
next: ./gauge.html
---

## ベンチマークアプリケーションの作成

まずは新たに **benchmarks** セクションを追加し、**criterion** という名前のベンチマークアプリを作ります。

```yaml
benchmarks:
  criterion:
    main: bench/Criterion.hs
    dependencies:
    - criterion
    - PFAD
```

ベンチマーク用のファイルを **bench/Criterion.hs** という名前で作ります。

```haskell
module Main (main) where

import Criterion.Main

import Minfree

main :: IO ()
main =
  defaultMain
    [ bgroup "fast minfree (whnf)"
        [ bench "n=1"     $ whnf minfree' [0..1]
        , bench "n=10"    $ whnf minfree' [0..10]
        , bench "n=100"   $ whnf minfree' [0..100]
        , bench "n=100"   $ whnf minfree' [0..1000]
        , bench "n=10000" $ whnf minfree' [0..10000]
        ]
    , bgroup "slow minfree (whnf)"
        [ bench "n=1"     $ whnf minfree [0..1]
        , bench "n=10"    $ whnf minfree [0..10]
        , bench "n=100"   $ whnf minfree [0..100]
        , bench "n=100"   $ whnf minfree [0..1000]
        , bench "n=10000" $ whnf minfree [0..10000]
        ]
    , bgroup "fast minfree (nf)"
        [ bench "n=1"     $ nf minfree' [0..1]
        , bench "n=10"    $ nf minfree' [0..10]
        , bench "n=100"   $ nf minfree' [0..100]
        , bench "n=100"   $ nf minfree' [0..1000]
        , bench "n=10000" $ nf minfree' [0..10000]
        ]
    , bgroup "slow minfree (nf)"
        [ bench "n=1"     $ nf minfree [0..1]
        , bench "n=10"    $ nf minfree [0..10]
        , bench "n=100"   $ nf minfree [0..100]
        , bench "n=100"   $ nf minfree [0..1000]
        , bench "n=10000" $ nf minfree [0..10000]
        ]
    ]
```

## ベンチマークの実行

実際にベンチマークを実行します。

```shell
$ stack bench
...
benchmarking fast minfree (whnf)/n=1
time                 49.89 ns   (49.81 ns .. 50.00 ns)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 51.96 ns   (49.90 ns .. 59.25 ns)
std dev              11.50 ns   (328.2 ps .. 23.97 ns)
variance introduced by outliers: 98% (severely inflated)

benchmarking fast minfree (whnf)/n=10
time                 352.6 ns   (350.9 ns .. 355.9 ns)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 352.8 ns   (351.2 ns .. 359.3 ns)
std dev              9.990 ns   (1.401 ns .. 21.00 ns)
variance introduced by outliers: 41% (moderately inflated)

benchmarking fast minfree (whnf)/n=100
time                 3.535 μs   (3.522 μs .. 3.548 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.530 μs   (3.520 μs .. 3.543 μs)
std dev              39.52 ns   (32.15 ns .. 52.44 ns)

benchmarking fast minfree (whnf)/n=100
time                 35.89 μs   (35.80 μs .. 35.98 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 36.05 μs   (35.98 μs .. 36.14 μs)
std dev              255.6 ns   (199.7 ns .. 353.0 ns)

benchmarking fast minfree (whnf)/n=10000
time                 411.1 μs   (409.4 μs .. 413.6 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 413.3 μs   (411.7 μs .. 415.1 μs)
std dev              5.656 μs   (4.510 μs .. 6.915 μs)

benchmarking slow minfree (whnf)/n=1
time                 66.88 ns   (66.76 ns .. 67.00 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 66.90 ns   (66.76 ns .. 67.08 ns)
std dev              520.9 ps   (365.0 ps .. 689.3 ps)

benchmarking slow minfree (whnf)/n=10
time                 875.6 ns   (873.3 ns .. 879.0 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 877.1 ns   (873.9 ns .. 888.3 ns)
std dev              17.15 ns   (5.492 ns .. 34.61 ns)
variance introduced by outliers: 23% (moderately inflated)

benchmarking slow minfree (whnf)/n=100
time                 43.77 μs   (43.68 μs .. 43.85 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 43.84 μs   (43.79 μs .. 43.92 μs)
std dev              225.2 ns   (160.3 ns .. 383.9 ns)

benchmarking slow minfree (whnf)/n=100
time                 3.989 ms   (3.967 ms .. 4.005 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 4.026 ms   (4.002 ms .. 4.114 ms)
std dev              112.5 μs   (19.23 μs .. 233.0 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking slow minfree (whnf)/n=10000
time                 394.3 ms   (390.5 ms .. 397.0 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 395.8 ms   (394.9 ms .. 396.7 ms)
std dev              1.066 ms   (892.6 μs .. 1.207 ms)
variance introduced by outliers: 19% (moderately inflated)
```

```shell
benchmarking fast minfree (nf)/n=1
time                 53.76 ns   (53.67 ns .. 53.88 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 53.65 ns   (53.57 ns .. 53.75 ns)
std dev              292.9 ps   (239.6 ps .. 363.6 ps)

benchmarking fast minfree (nf)/n=10
time                 356.3 ns   (355.7 ns .. 357.2 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 357.2 ns   (356.3 ns .. 358.4 ns)
std dev              3.472 ns   (2.622 ns .. 4.903 ns)

benchmarking fast minfree (nf)/n=100
time                 3.512 μs   (3.503 μs .. 3.524 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.533 μs   (3.524 μs .. 3.544 μs)
std dev              35.25 ns   (28.99 ns .. 42.14 ns)

benchmarking fast minfree (nf)/n=100
time                 39.49 μs   (39.34 μs .. 39.63 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 39.36 μs   (39.17 μs .. 39.54 μs)
std dev              587.7 ns   (501.1 ns .. 699.9 ns)
variance introduced by outliers: 10% (moderately inflated)

benchmarking fast minfree (nf)/n=10000
time                 414.1 μs   (411.6 μs .. 416.6 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 420.0 μs   (413.8 μs .. 442.8 μs)
std dev              36.40 μs   (5.545 μs .. 75.12 μs)
variance introduced by outliers: 71% (severely inflated)

benchmarking slow minfree (nf)/n=1
time                 69.84 ns   (69.73 ns .. 69.96 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 69.81 ns   (69.70 ns .. 70.09 ns)
std dev              497.2 ps   (217.2 ps .. 966.7 ps)

benchmarking slow minfree (nf)/n=10
time                 876.0 ns   (874.9 ns .. 877.5 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 877.8 ns   (876.7 ns .. 879.6 ns)
std dev              4.789 ns   (3.518 ns .. 7.251 ns)

benchmarking slow minfree (nf)/n=100
time                 43.71 μs   (43.64 μs .. 43.77 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 43.75 μs   (43.73 μs .. 43.78 μs)
std dev              98.61 ns   (84.19 ns .. 124.2 ns)

benchmarking slow minfree (nf)/n=100
time                 4.005 ms   (3.988 ms .. 4.017 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 4.024 ms   (4.004 ms .. 4.080 ms)
std dev              94.24 μs   (15.66 μs .. 193.4 μs)

benchmarking slow minfree (nf)/n=10000
time                 395.7 ms   (390.8 ms .. 399.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 396.6 ms   (395.7 ms .. 397.6 ms)
std dev              1.099 ms   (899.0 μs .. 1.265 ms)
variance introduced by outliers: 19% (moderately inflated)
```

**nf** や **whnf** の詳細はここでは語り切れないトピックなので、気になる人は[正格性のすべて (翻訳)](/posts/2018/06-25-All-About-Strictness.html)を読んでください。

### html レポートの出力

**html** のレポートを取得するためにはオプションを別途指定します。

```haskell
module Main (main) where

import Criterion.Main
import Criterion.Types -- 追加

import Minfree

main :: IO ()
main =
  defaultMainWith (defaultConfig {reportFile=Just "./report.html"})  -- この行を変更
    [ bgroup "fast minfree (whnf)"
        [ bench "n=1"     $ whnf minfree' [0..1]
        , bench "n=10"    $ whnf minfree' [0..10]
        , bench "n=100"   $ whnf minfree' [0..100]
        , bench "n=100"   $ whnf minfree' [0..1000]
        , bench "n=10000" $ whnf minfree' [0..10000]
        ]
    , bgroup "slow minfree (whnf)"
        [ bench "n=1"     $ whnf minfree [0..1]
        , bench "n=10"    $ whnf minfree [0..10]
        , bench "n=100"   $ whnf minfree [0..100]
        , bench "n=100"   $ whnf minfree [0..1000]
        , bench "n=10000" $ whnf minfree [0..10000]
        ]
    , bgroup "fast minfree (nf)"
        [ bench "n=1"     $ nf minfree' [0..1]
        , bench "n=10"    $ nf minfree' [0..10]
        , bench "n=100"   $ nf minfree' [0..100]
        , bench "n=100"   $ nf minfree' [0..1000]
        , bench "n=10000" $ nf minfree' [0..10000]
        ]
    , bgroup "slow minfree (nf)"
        [ bench "n=1"     $ nf minfree [0..1]
        , bench "n=10"    $ nf minfree [0..10]
        , bench "n=100"   $ nf minfree [0..100]
        , bench "n=100"   $ nf minfree [0..1000]
        , bench "n=10000" $ nf minfree [0..10000]
        ]
    ]
```

このプログラムを実行すると、ベンチマークの結果とともに **report.html** というファイルがカレントディレクトリに出力されます。

[実際に出力された report.html はこちらで確認できます。](/images/report.html)
