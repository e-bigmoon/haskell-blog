---
title: Haskell Language Server のインストール
author: Wataru Yamada
tags: bigmoon
---

HLS(Haskell Language Server) を **手動で** インストールする方法を説明します。

HLSを **自動で** インストールする方法は以下のPRで開発中です。

- [GitHub Action static binaries and runtime libdir](https://github.com/haskell/haskell-language-server/pull/165)
- [Automatic installation of haskell-language-server binaries](https://github.com/alanz/vscode-hie-server/pull/236)

<!--more-->

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

準備ができたらHLSを[リポジトリ][hls-github]からクローンしてインストールしましょう。(以下の例では **GHC-8.8.3** を対象としています。)

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

## エディタの設定

### VSCode

拡張機能 [Haskell Language Server](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server) をインストールし、
設定から `haskell-language-server` を指定します。

![VSCodeの設定](/images/2020/07-12/hls-vscode.png)

### Emacs

[こちら](/hie/emacs.html)の記事を参考に、
[lsp-mode](https://github.com/emacs-lsp/lsp-mode),
[lsp-ui](https://github.com/emacs-lsp/lsp-ui),
[lsp-haskell](https://github.com/emacs-lsp/lsp-haskell)
をEmacsにインストールします。

設定ファイルで以下のように `haskell-language-server-wrapper` を指定します。

```
(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;;(setq lsp-log-io t)
)
```

### 参考

- [haskell-language-server](https://github.com/haskell/haskell-language-server)
- [Emacs で Haskell IDE Engine を使う](/hie/emacs.html)