---
title: stackの更新
---

```shell-session
$ stack upgrade

$ stack --version
Version 1.6.1, Git revision f25811329bbc40b0c21053a8160c56f923e1201b (5435 commits) x86_64 hpack-0.20.0

# バージョン指定 (元に戻したい場合などに便利)
$ stack upgrade --binary-version 1.5.1
```

似たコマンドに `stack update` というものがありますが、こちらはほぼ利用しません。なぜなら `stack update` は `cabal update` が行うようにパッケージインデックスの更新を明示的に行うコマンドですが、必要であれば `stack` の内部で自動的に `stack update` が実行されるためです。
=> [How do I update my package index?](https://github.com/commercialhaskell/stack/blob/master/doc/faq.md#how-do-i-update-my-package-index)

また `stack upgrade` を行うと、既存とは異なるパスにインストールされる点に注意してください。

```shell
$ curl -sSL https://get.haskellstack.org/ | sh
$ which stack
/usr/local/bin/stack

$ stack upgrade
$ which stack
/home/bm12/.local/bin/stack

# 基本的にこの設定をしておいた方が良い
# export PATH=$PATH:~/.local/bin とすると、古い stack を認識するので注意
$ export PATH=~/.local/bin:$PATH
```
