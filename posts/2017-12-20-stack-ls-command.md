---
title: stack ls コマンドが追加されます
author: Shinya Yamaguchi
tags: bigmoon, stack
---

# stack ls コマンド

`Stack` の `master` に `stack ls` コマンドがマージされました。

- [Add stack ls snapshots documentation to the user guide #3672](https://github.com/commercialhaskell/stack/pull/3672)

今後、このコマンドのサブコマンドに `list-dependencies` などを順次追加していく方針?のようです。

- [Stack list-dependencies : Bring it under the new ls umbrella command #3669](https://github.com/commercialhaskell/stack/issues/3669)

<!--more-->

## 使ってみよう！

このコマンドを使うためには `stack` の `master` ブランチの最新版をインストールする必要があります。

```bash
$ stack upgrade --git
# ソースからコンパイルするので時間がかかります

$ stack --version
Version 1.7.0, Git revision 7d68bd695c4de8f231a95e66d0c882031f8255de (5468 commits) x86_64 hpack-0.20.0
```

現状、実装されているコマンドとしては `stack ls snapshots` があります。

```bash
# ローカルのスナップショット一覧を表示
$ stack ls snapshots

# lts のみ表示 (lts の略)
$ stack ls snapshots -l

# nightly のみ表示 (nightly の略)
$ stack ls snapshots -n

# リモートのスナップショット一覧を表示
$ stack ls snapshots remote

$ stack ls snapshots -l remote

$ stack ls snapshots -n remote
```

実行すると `less` が立ち上がり、結果を確認することができます。

## まとめ

これで [Stackage](https://www.stackage.org/) を見なくても、どんなスナップショットが利用できるか、すぐに確認できるようになりますね。