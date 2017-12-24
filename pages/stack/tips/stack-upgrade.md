---
title: stack の更新
date: 2017/12/24
---

## 最新の安定版へアップグレードする方法

```shell
$ stack upgrade

$ stack --version
```

`stack` のバイナリファイルがダウンロードされるため、更新はすぐに終わります。

## 最新の開発版へアップグレードする方法

`github` の最新版を利用したい方は次のコマンドでアップグレードしましょう。

```shell
$ stack upgrade --git
```

この場合、ソースコードからのコンパイルとなるため、比較的時間がかかります。(30分以上)

また、上記の場合は `master` の最新版を取得します。

### ブランチの変更

`master` 以外のブランチを指定したい場合は、以下のように `--git-branch` オプションにブランチ名を指定すると良いでしょう。

```shell
$ stack upgrade --git --git-branch release
```

## stack のバージョンを指定してアップグレードする方法

```shell
$ stack upgrade --binary-version 1.5.1
```

`--binary-version` オプションを使うことで、以前のバージョンにダウングレードさせることも可能です。

### stack update コマンドについて

`stack upgrade` と似たコマンドに `stack update` というものがありますが、こちらはほぼ利用しません。

なぜなら `stack update` は `cabal update` が行うようにパッケージインデックスの更新を明示的に行うコマンドですが、必要であれば `stack` の内部で自動的に `stack update` が実行されるためです。

- [How do I update my package index?](https://github.com/commercialhaskell/stack/blob/master/doc/faq.md#how-do-i-update-my-package-index)


## 注意点

`stack upgrade` を行うと、既存とは異なるパスにインストールされる点に注意してください。

```shell
$ curl -sSL https://get.haskellstack.org/ | sh
$ which stack
/usr/local/bin/stack

$ stack upgrade
$ which stack
/home/bm12/.local/bin/stack
```

基本的には、以下のような `PATH` の設定をしておいた方が良いと思います。

```shell
$ export PATH=~/.local/bin:$PATH
```

反対に `export PATH=$PATH:~/.local/bin` とすると、古い `stack` を参照してしまうため、注意が必要です。

`stack` の更新が完了した場合、常にバージョンを確認する方が良いでしょう。