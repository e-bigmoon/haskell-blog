---
title: Haskell Language Server のインストール
author: Wataru Yamada
tags: bigmoon
---

HLS(Haskell Language Server) のインストール方法

## 実行環境

| 環境  | バージョン   |
|:-----:|:-------------|
| OS    | Ubuntu 18.04 |
| Stack |        2.3.1 |
| HLS   | ghcide version: 0.2.0.0 (GHC: 8.8.3) (PATH: ~/.local/bin/haskell-language-server) (GIT hash: 768fdcd588f5c37839086c0d787d1cb7438de37b) |

## 導入手順

### 1. HLS のインストール

インストールには以下のものが必要です。

- [stack](https://docs.haskellstack.org/en/stable/README/) (バージョン2.1.1以上)
  - または、[cabal](https://www.haskell.org/cabal/users-guide/)
- `icu` のライブラリなど

必要に応じてインストールしておきましょう。

```shell
$ stack upgrade
$ sudo apt update
$ sudo apt install libicu-dev libncurses-dev libgmp-dev zlib1g-dev
```

準備ができたらHIEを[リポジトリ][hls-github]からクローンしてインストールしましょう。(以下の例では **GHC-8.8.3** を対象としています。)

```shell
$ git clone https://github.com/haskell/haskell-language-server --recurse-submodules
$ cd haskell-language-server
$ stack ./install.hs hls
```

[hls-github]: https://github.com/haskell/haskell-language-server

インストールができました。

```shell
$ haskell-language-server --version
ghcide version: 0.2.0.0 (GHC: 8.8.3) (PATH: /home/yamada/.local/bin/haskell-language-server) (GIT hash: 768fdcd588f5c37839086c0d787d1cb7438de37b)
```

### 参考

- [haskell-language-server](https://github.com/haskell/haskell-language-server)
