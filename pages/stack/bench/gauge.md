---
title: gauge パッケージ
date: 2019/09/15
prev: ./criterion.html
next: ../dist/index.html
---

## ベンチマークアプリケーションの作成

**gauge** の API は **criterion** と互換性があるので、ほとんどプログラムはそのままです。

```yaml
benchmarks:
  criterion:
    main: bench/Criterion.hs
    dependencies:
    - criterion
    - PFAD
  # ここから下の行を追記
  gauge:
    main: bench/Gauge.hs
    dependencies:
    - gauge
    - PFAD
```

```haskell
module Main (main) where

import Gauge

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

## ベンチマークアプリケーションの実行

ベンチマークを実行します。

```shell
$ stack bench PFAD:gauge
...
内容は criterioin の時とほとんど同じなので省略します

benchmarked slow minfree (nf)/n=10000
time                 451.6 ms   (451.0 ms .. 451.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 451.5 ms   (451.1 ms .. 451.8 ms)
std dev              580.0 μs   (384.2 μs .. 866.7 μs)

Benchmark gauge: FINISH
```

### displayMode オプション

**displayMode** を **Condensed** にすると表示結果がコンパクトになります。

```haskell
module Main (main) where

import Gauge

import Minfree

main :: IO ()
main =
  defaultMainWith (defaultConfig {displayMode=Condensed}) -- この行を変更
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

```shell
$ stack bench PFAD:gauge
Running 1 benchmarks...
Benchmark gauge: RUNNING...
fast minfree (whnf)/n=1                  mean 51.34 ns  ( +- 12.79 ns  )
fast minfree (whnf)/n=10                 mean 358.6 ns  ( +- 2.252 ns  )
fast minfree (whnf)/n=100                mean 3.662 μs  ( +- 38.80 ns  )
fast minfree (whnf)/n=100                mean 36.86 μs  ( +- 1.720 μs  )
fast minfree (whnf)/n=10000              mean 420.7 μs  ( +- 5.488 μs  )
slow minfree (whnf)/n=1                  mean 69.98 ns  ( +- 416.9 ps  )
slow minfree (whnf)/n=10                 mean 955.2 ns  ( +- 10.19 ns  )
slow minfree (whnf)/n=100                mean 49.72 μs  ( +- 151.0 ns  )
slow minfree (whnf)/n=100                mean 4.583 ms  ( +- 107.5 μs  )
benchmarking slow minfree (whnf)/n=10000 ... took 24.86 s, total 56 iterations
slow minfree (whnf)/n=10000              mean 451.5 ms  ( +- 781.8 μs  )
fast minfree (nf)/n=1                    mean 50.68 ns  ( +- 378.0 ps  )
fast minfree (nf)/n=10                   mean 359.9 ns  ( +- 17.51 ns  )
fast minfree (nf)/n=100                  mean 3.663 μs  ( +- 34.37 ns  )
fast minfree (nf)/n=100                  mean 36.49 μs  ( +- 270.0 ns  )
fast minfree (nf)/n=10000                mean 420.0 μs  ( +- 5.958 μs  )
slow minfree (nf)/n=1                    mean 71.65 ns  ( +- 875.4 ps  )
slow minfree (nf)/n=10                   mean 957.9 ns  ( +- 10.72 ns  )
slow minfree (nf)/n=100                  mean 50.05 μs  ( +- 257.0 ns  )
slow minfree (nf)/n=100                  mean 4.604 ms  ( +- 156.4 μs  )
benchmarking slow minfree (nf)/n=10000 ... took 24.88 s, total 56 iterations
slow minfree (nf)/n=10000                mean 451.9 ms  ( +- 1.068 ms  )
Benchmark gauge: FINISH
```

### quickMode オプション

ベンチマークは与えられた引数でプログラムを何度か動かすため、ベンチマークの実行に時間がかかりすぎる場合あります。

とりあえず、すぐに結果が欲しい場合は **quickMode** オプションを **True** に設定しましょう。

```haskell
module Main (main) where

import Gauge

import Minfree

main :: IO ()
main =
  defaultMainWith (defaultConfig {displayMode=Condensed, quickMode=True}) -- この行を変更
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

```shell
$ stack bench PFAD:gauge
...
Running 1 benchmarks...
Benchmark gauge: RUNNING...
fast minfree (whnf)/n=1                  time                 132.7 ns  

fast minfree (whnf)/n=10                 time                 360.9 ns  

fast minfree (whnf)/n=100                time                 3.657 μs  

fast minfree (whnf)/n=100                time                 38.08 μs  

fast minfree (whnf)/n=10000              time                 435.5 μs  

slow minfree (whnf)/n=1                  time                 69.99 ns  

slow minfree (whnf)/n=10                 time                 958.4 ns  

slow minfree (whnf)/n=100                time                 50.00 μs  

slow minfree (whnf)/n=100                time                 4.593 ms  

slow minfree (whnf)/n=10000              time                 451.4 ms  

fast minfree (nf)/n=1                    time                 50.65 ns  

fast minfree (nf)/n=10                   time                 362.7 ns  

fast minfree (nf)/n=100                  time                 3.683 μs  

fast minfree (nf)/n=100                  time                 37.21 μs  

fast minfree (nf)/n=10000                time                 423.2 μs  

slow minfree (nf)/n=1                    time                 71.52 ns  

slow minfree (nf)/n=10                   time                 967.8 ns  

slow minfree (nf)/n=100                  time                 50.88 μs  

slow minfree (nf)/n=100                  time                 4.581 ms  

slow minfree (nf)/n=10000                time                 454.5 ms  

Benchmark gauge: FINISH
```
