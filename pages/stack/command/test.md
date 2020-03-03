---
title: stack test
published: 2018/07/30
# updated: 2019/09/15
---

## 目的

**ビルド**と**テスト**を行うためのコマンドです。非常によく使います。

## 使い方

全てのテストを実行

```shell
$ stack test --fast
```

またテストコンポーネントが複数ある場合は以下のようにして、テスト対象を限定することもできます。例えば、テストは実行したいけど、毎回 haddock のテスト走らせたくないという場合などで便利です。

```shell
$ stack test --fast <package>:test:<component>
```

## テスト時に引数を指定する方法

テスト時に引数を渡したい場合は `--test-arguments` オプションを使います。

```shell
$ stack test --test-arguments="<option>"
```

実際にはプロファイルのオプションを指定する際や、 `tasty-html` などで使う場合があります。

```shell
$ stack test --profile --test-arguments "+RTS -hm"
$ stack test --test-arguments="--html results.html"
```