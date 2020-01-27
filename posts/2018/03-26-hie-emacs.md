---
title: Haskell IDE Engine を Emacs で使う
author: Wataru Yamada
tags: bigmoon, package
---

## はじめに

[HIE (Haskell IDE Engine)](https://github.com/haskell/haskell-ide-engine) を `Emacs` に導入する方法が `HIE` の [README.md](https://github.com/haskell/haskell-ide-engine/pull/502/files) に追記されたので、それに従いインストールしてみました。

本記事では、導入手順を紹介します。

追記 (2020/01/27): 本記事の内容は古くなっている可能性があるため、継続的にメンテナンスしている「[Emacs で Haskell IDE Engine を使う](/hie/emacs.html)」ページを参照してください。

<!--more-->

## 導入手順

### 1. HIE をインストールする。

```bssh
$ git clone https://github.com/haskell/haskell-ide-engine.git
$ cd haskell-ide-engine
$ make

...

* Missing C libraries: icuuc, icui18n, icudata
```

`make` 実行時、以下のようにライブラリが足りなかったので、必要なパッケージをインストールして、もう一度 `make` しました。

```bash
$ sudo apt install libicu-dev
$ make
```

普通に `stack install` で導入することもできますが、 `HIE` はビルド時の `GHC` バージョンでしか使えないという弱点があるため、`GHC` の バージョンが異なるプロジェクトでは利用できません。

そのため `make` を使って `hie-8.2.1` のように `GHC` のそれぞれのバージョンでビルドした `HIE` のバイナリを用意して解決しています。

### 2. 必要なパッケージを入手する。

```bash
$ git clone git@github.com:emacs-lsp/lsp-mode.git
$ git clone git@github.com:emacs-lsp/lsp-ui.git
$ git clone git@github.com:emacs-lsp/lsp-haskell.git
```

### 3. emacs の設定ファイル (init.el など) に以下を追加する。

```elisp
(add-to-list 'load-path "/path/to/lsp-mode")
(add-to-list 'load-path "/path/to/lsp-ui")
(add-to-list 'load-path "/path/to/lsp-haskell")

(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp-haskell)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'haskell-mode-hook #'lsp-haskell-enable)
(add-hook 'haskell-mode-hook 'flycheck-mode)
```

上記内容を記述した後で Emacs を起動したときに、依存するパッケージのインストールを要求される場合があるので、`Cask` や `package-install` などでインストールしましょう。

インストールが成功すると、`Haskell` のファイルを読み込んだときに、カーソルを当てたところにある関数の型の表示、エラーの表示、補完、コマンド `M-.` で定義のところにジャンプなどができるようになります。

## おわりに

`HIE` は `ghc-mod` の実行ファイルを使っているかのように見えますが、実際はライブラリを通しているだけでバイナリは使っていないそうです。
