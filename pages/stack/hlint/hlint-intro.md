---
title: HLintの導入と実行
published: 2018/02/10
updated: 2018/04/07
---

## HLint の導入

`HLint` は以下のコマンドで簡単に導入できます。

```shell
$ stack install hlint

$ hlint --version
HLint v2.1.1, (C) Neil Mitchell 2006-2018
```

現在の最新版は `v2.1.1` となっています。`HLint` のバージョンによって出力内容が異なることが良くありますのでご注意ください。

また、お試しで使ってみたい人は以下のコマンドを実行してみましょう。カレントディレクトリ以下のファイルが検査されます。

```shell
$ curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .
...
```

## HLint の実行方法

`HLint` はディレクトリを指定すると再帰的に解析を行ってくれます。

```shell
# プロジェクト全体に対して再帰的に実行
$ hlint .
```

```shell
# 特定のディレクトリ (src) に対して再帰的に実行
$ hlint src
```

```shell
# 複数のディレクトリ (src, test) に対して再帰的に実行
$ hlint src test
```

```shell
# 単一のファイル (app/Main.hs) にのみ実行
$ hlint app/Main.hs
```