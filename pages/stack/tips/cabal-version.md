---
title: cabal のバージョンについて
date: 2019/09/14
published: 2019/09/14
# updated: 2019/12/24
---

cabal のバージョンを確認するためには `--version` オプションを利用します。

```shell
$ cabal --version
cabal-install version 3.0.0.0
compiled using version 3.0.0.0 of the Cabal library
```

同様に短縮系の `-V` も利用できます。

```shell
$ cabal -V
cabal-install version 3.0.0.0
compiled using version 3.0.0.0 of the Cabal library
```

## numeric-version オプション

バージョンの数字だけを表示する場合は `--numeric-version` オプションを使います。

```shell
$ cabal --numeric-version
3.0.0.0
```