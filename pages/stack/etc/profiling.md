---
title: プロファイリング
date: 2018/04/07
---

## プロジェクトのプロファイル

### 実行ファイル

```shell
$ stack build --profile
$ stack exec -- <bin_name> +RTS -p -hc
$ stack exec -- hp2ps -e8in -c <proj_name>.hp
```

### テスト

引数を渡すためには `--test-arguments` を使う必要があります。

```shell
$ stack test --profile --test-arguments "+RTS -hm"
```

## 単一ファイルのプロファイル

`Example.hs` を実行し、メモリプロファイルを取得する例。

```shell
$ stack ghc Example && sudo ./Example +RTS -s
```

## プロファイル結果

```shell
13,440,124,048 bytes allocated in the heap
 8,760,418,592 bytes copied during GC
 1,225,650,008 bytes maximum residency (23 sample(s))
    19,423,400 bytes maximum slop
          2599 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
Gen  0      9869 colls,     0 par    7.821s   9.831s     0.0010s    1.1223s
Gen  1        23 colls,     0 par    0.011s   0.013s     0.0006s    0.0009s

INIT    time    0.000s  (  0.000s elapsed)
MUT     time    5.255s  (  6.347s elapsed)
GC      time    7.832s  (  9.845s elapsed)
EXIT    time    0.032s  (  0.123s elapsed)
Total   time   13.118s  ( 16.315s elapsed)

%GC     time      59.7%  (60.3% elapsed)

Alloc rate    2,557,607,298 bytes per MUT second

Productivity  40.3% of total user, 39.7% of total elapsed
```