---
title: stack-2.1 から location に extra-dep を指定できなくなります。
author: Shinya Yamaguchi
tags: bigmoon, stack
updated: 202003/25
---

## はじめに

まだまだ先の話ですが、`extra-dep` の書き方が **Legacy syntax** になりました。`stack-2.1` からはビルドできなくなります。

以下の設定例は[公式ドキュメント](https://github.com/commercialhaskell/stack/blob/master/doc/yaml_configuration.md#packages)の例を少し変更したものです。

```yaml
packages:
- .
- location:
    git: https://github.com/bitemyapp/esqueleto.git
    commit: 08c9b4cdf977d5bcd1baba046a007940c1940758
  extra-dep: true
- location:
    ./submodules/haskell-lsp
  extra-dep: true
  subdirs:
    - .
    - haskell-lsp-types

extra-deps:
  - streaming-commons-0.2.0.0
```

修正方法は、設定ファイルの `packages` に `extra-dep: true` と指定している部分を、以下のように `extra-deps` へ追加するだけです。また、 `subdirs` を指定している場合は単純に分割して指定します。

```yaml
packages:
- .

extra-deps:
  - git: https://github.com/bitemyapp/esqueleto.git
    commit: 08c9b4cdf977d5bcd1baba046a007940c1940758
  - ./submodules/haskell-lsp
  - ./submodules/haskell-lsp/haskell-lsp-types
  - streaming-commons-0.2.0.0
```

`extra-deps` の構文は従来の `stack` でも使えるので、今から対応しておいても問題ないと思います。

以上です。

<!--more-->