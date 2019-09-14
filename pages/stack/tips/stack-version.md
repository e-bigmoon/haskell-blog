---
title: Stack のバージョンについて
date: 2019/09/14
---

stack のバージョンを確認するためには `--version` オプションを利用します。

```shell
$ stack --version
Version 2.1.3, Git revision 636e3a759d51127df2b62f90772def126cdf6d1f (7735 commits) x86_64 hpack-0.31.2
```

上記の結果から、以下のバージョンを利用していることがわかります。

_ | バージョン
----|----
stack | 2.1.3
hpack | 0.31.2

## その他の確認方法

## numeric-version オプション

`stack` のバージョンのみを表示します。

```shell
stack --numeric-version
2.1.3
```

## hpack-numeric-version オプション

`hpack` のバージョンのみを表示します。

```shell
$ stack --hpack-numeric-version
0.31.2
```