---
title: Stack の更新
date: 2019/09/14
---

## stack のアップグレード方法

最新の **stack** を利用するためには、以下のコマンドを実行します。

```shell
$ stack upgrade

$ stack --version
```

**stack** のバイナリファイルがダウンロードされるため、更新はすぐに終わります。

## その他のアップグレード方法

- 開発中の **stack** を利用したい方は、次のコマンドでアップグレードすることができます。

```shell
$ stack upgrade --git
```

この場合、ソースコードからのコンパイルとなるため、比較的時間がかかります。(30分以上)

また、上記の場合は [master](https://github.com/commercialhaskell/stack/commits/master) の最新版を取得します。

- **master** 以外のブランチを指定したい場合は、以下のように `--git-branch` オプションにブランチ名を指定します。

```shell
$ stack upgrade --git --git-branch release
```

- **stack** のバージョンを指定してアップグレードする場合は、`--binary-version` オプションを利用します。

```shell
# バージョン指定 (前のバージョンに戻したい場合などに便利)
$ stack upgrade --binary-version 2.1.1
```

過去のバージョンを指定することで、stack をダウングレードさせることも可能です。

## stack update コマンドについて

**stack upgrade** と似たコマンドに **stack update** というものがありますが、こちらはほぼ利用しません。

なぜなら **stack update** は **cabal update** が行うようにパッケージインデックスの更新を明示的に行うコマンドですが、必要であれば **stack** の内部で自動的に **stack update** が実行されるためです。

- [How do I update my package index?](https://github.com/commercialhaskell/stack/blob/master/doc/faq.md#how-do-i-update-my-package-index)

## 注意点

**stack upgrade** を行うと既存とは異なるパスにインストールされる場合があります。

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
# OK
$ export PATH=~/.local/bin:$PATH
```

順序を逆にしてしまうととすると、常に古い **stack** を参照してしまうため、注意が必要です。

```shell
# NG
$ export PATH=$PATH:~/.local/bin
```

このようなミスを防ぐためには **stack** の更新が完了したら、常に **stack** のバージョンを確認しましょう。

```shell
$ stack --version
Version 2.1.3, Git revision 0fa51b9925decd937e4a993ad90cb686f88fa282 (7739 commits) x86_64 hpack-0.31.2
```
