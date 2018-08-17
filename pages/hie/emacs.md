---
title: Emacs で Haskell IDE Engine を使う
date: 2018/08/17
---

## 実行環境

| 環境  | バージョン   |
|:-----:|:-------------|
| OS    | Ubuntu 17.10 |
| Stack |        1.7.1 |
| HIE   |      0.2.2.0 |
| Emacs |         26.1 |

## 導入手順

### 1. HIE のインストール

インストールには以下のものが必要です。

- `stack`(バージョン1.7.1以上)
- `cabal-install`
- `icu` のライブラリなど

必要に応じてインストールしておきましょう。

```sh
$ stack upgrade
$ stack install cabal-install
$ sudo apt install libicu-dev libtinfo-dev
```

準備ができたらHIEをリポジトリからクローンしてインストールしましょう。

```sh
$ git clone https://github.com/haskell/haskell-ide-engine --recursive
$ cd haskell-ide-engine
$ make build-all
```

### 2. 必要なパッケージの入手

EmacsでHIEを使うには次の３つのパッケージが必要です。

- [lsp-mode](https://github.com/emacs-lsp/lsp-mode)
- [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
- [lsp-haskell](https://github.com/emacs-lsp/lsp-haskell)

これらのパッケージをインストールする場所に移動します。次に行う設定ファイルの更新でインストール場所を指定するのでわかりやすい場所に移動しておきましょう。おすすめは `~/.emacs.d/elisp` です。インストールはクローンするだけです。

```sh
$ cd /path/to/install
$ git clone https://github.com/emacs-lsp/lsp-mode
$ git clone https://github.com/emacs-lsp/lsp-ui
$ git clone https://github.com/emacs-lsp/lsp-haskell
```

### 3. Emacs の設定ファイルを更新

最後に設定ファイルを更新します。Emacs の設定ファイル名は `~/.emacs`, `~/.emacs.el`, `~/.emacs.d/init.el` のどれか１つなら大丈夫です。
設定ファイルに以下の内容を記述すれば Emacs で HIE が使えるようになります。
最初の３行でで先程インストールした３つのパッケージの場所を指定してください。

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

### 補足1 : Emacs26を使ってドキュメントをきれいに表示する

HIEがドキュメントなどを表示とする時、現在編集しているバッファ割り込んで表示しようとしますが、Emacs26で追加された機能 `child flame` を使うことで割り込まずにきれいに表示できます。次のコマンドでインストールできます。

```sh
$ sudo add-apt-repository ppa:kelleyk/emacs
$ sudo apt-get update
$ sudo apt install emacs26
```

すでにEmacs25などがインストール済みで `emacs` コマンドで `emacs26` が開けない場合は次のコマンドで変更しましよう。

```sh
$ sudo update-alternatives --config emacs

```

![きれいに表示されたHIEの画像](/images/hie-emacs.png)

### 補足2 : ビルドはできたのにHIEが動かない

HIEのビルドや必要なパッケージのインストールなどを正しく行ったにもかかわらず、次のようにEmacsにエラーが表示されてフリーズしてしまう、ということがありました。

```
[1 of 5] Compiling CabalHelper.Common ( CabalHelper/Common.hs, /home/yamada/.ghc-mod/cabal-helper/CabalHelper/Common.o )
[2 of 5] Compiling CabalHelper.Licenses ( CabalHelper/Licenses.hs, /home/yamada/.ghc-mod/cabal-helper/CabalHelper/Licenses.o )

CabalHelper/Licenses.hs:56:8: error:
    • Expecting one more argument to ‘CPackageIndex ModuleName’
      Expected a type, but ‘CPackageIndex ModuleName’ has kind ‘* -> *’
    • In the type signature:
        findTransitiveDependencies :: CPackageIndex ModuleName
                                      -> Set CInstalledPackageId -> Set CInstalledPackageId
   |
56 |     :: CPackageIndex Distribution.ModuleName.ModuleName
   |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
ghc-mod: GMEProcess "readProcessStderrChan" "/home/yamada/.stack/snapshots/x86_64-linux/lts-9.18/8.0.2/libexec/x86_64-linux-ghc-8.0.2/cabal-helper-0.7.3.0/cabal-helper-wrapper" ["--with-ghc=/home/yamada/.stack/programs/x86_64-linux/ghc-8.4.3/bin/ghc","--with-ghc-pkg=/home/yamada/.stack/programs/x86_64-linux/ghc-8.4.3/bin/ghc-pkg","--with-cabal=cabal","/home/yamada/market-scratch","/home/yamada/market-scratch/.stack-work/dist/x86_64-linux/Cabal-2.2.0.1","package-db-stack","entrypoints","source-dirs","ghc-options","ghc-src-options","ghc-pkg-options","ghc-merged-pkg-options","ghc-lang-options","licenses","flags","config-flags","non-default-config-flags","compiler-version"] (Left 1)
```

おそらく、原因は古いバージョンの`ghc-mod`なので、次のように一度削除してからもう一度ビルドしたらなおりました。

```
rm -rf ~/.local/bin/ghc-mod
rm -rf ~/.stack/snapshots/x86_64-linux/lts-9.18
stack clean --full
make build-all
```

### 参考

- [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine)
- [Emacs gets stuck on loading file with HIE](https://github.com/haskell/haskell-ide-engine/issues/750)
- [Haskell IDE Engine を Emacs で使う](https://haskell.e-bigmoon.com/posts/2018/03-26-hie-emacs.html) (古い記事)
