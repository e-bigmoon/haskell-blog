---
title: VS Code と haskell-ide-engine で Haskell 開発環境を構築する
published: 2020/04/02
updated: 2020/08/28
---

---

現在は [haskell-language-server (hls)](https://github.com/haskell/haskell-language-server) に開発が移っており、こちらの方が機能も多く、インストールについても**ビルド不要**でバイナリが自動的に落ちてきます。

**hls** の動作は hie と比べてかなり安定しています。(windows 環境でも問題無く動いているようです)

インストール方法等については [Haskell環境構築2020簡易版 (macOS, Linux向け)](https://scrapbox.io/LugendrePublic/Haskell%E7%92%B0%E5%A2%83%E6%A7%8B%E7%AF%892020%E7%B0%A1%E6%98%93%E7%89%88_(macOS,_Linux%E5%90%91%E3%81%91)) が参考になります。

補足: 現状 **hie.yaml** ファイルは [Avi-D-coder/implicit-hie](https://github.com/Avi-D-coder/implicit-hie) を使って自動生成した方が良いです。

```shell
# for cabal
$ cabal install -z implicit-hie

# for stack
$ stack install implicit-hie

$ gen-hie > hie.yaml
```

また、不要になった **hie** のバイナリ等は以下のコマンドで削除できます。

```shell
# for cabal
$ rm ~/.cabal/bin/hie*

# for stack
$ rm ~/.local/bin/hie*
```

**hls** と周辺ツールとの関係は [The State of Haskell IDEs](https://mpickering.github.io/ide/posts/2020-05-08-state-of-haskell-ide.html) を読むとよくわかります。

---

**注意: 本記事の内容は古くなっているため Haskell 開発環境の構築方法として推奨していません。**

<div style="margin-bottom: 20em;"></div>

---

![本家のSSを引用](https://camo.githubusercontent.com/fb828845a665d6bff30340ba61bc9744013773a7/687474703a2f2f692e696d6775722e636f6d2f41637659524f762e676966)

この画像は本家リポジトリのスクリーンショットを引用しています。

現在リリースされている最新バージョンは [v1.2][hie-v12] です。(HIE はまだ安定して動作しない場合があるのでご注意ください)

この記事は以下のリビジョンで動作確認しています。

```shell
$ git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
$ cd haskell-ide-engine
$ git rev-parse HEAD
02815bb04d9ea3501e941ac116673f0968e1d0dd
```

- [v1.2 の変更点][hie-v12-changelog]

また、[vscode-hie-server][vscode-hie-server-repo] の最新バージョンは **0.0.37** です。

[hie-v12]: https://github.com/haskell/haskell-ide-engine/releases
[hie-v12-changelog]: https://github.com/haskell/haskell-ide-engine/blob/master/Changelog.md#12
[vscode-hie-server-repo]: https://github.com/alanz/vscode-hie-server/

## 実行環境

| 環境  | バージョン   |
|:-----:|:-------------|
| OS    | Ubuntu 18.04.4 LTS |
| Stack |        2.1.3 |
| HIE   | Version 1.2 x86_64 ghc-8.8.2 |
| vscode | 1.43.2 |

## haskell-ide-engine のビルド時間めちゃ長い問題について

現状、公式からビルド済みのバイナリは配布されていません。

「[Nix で Haskell IDE Engine をシュッと入れる][vscode-install-nix]」の方法でインストールすれば比較的短時間で終わるみたいです。興味ある方は試してみると良いかもしれません！

また、その他にも VS Code の Devcontainer 機能を使ったインストール方法もあります。

- [Installation with GHC and HIE as a VS Code Devcontainer][hie-install-devcon-repo]
- [Setting up a Haskell development environment in minutes in Visual Studio][hie-install-devcon]

[hie-install-nix]: https://ryota-ka.hatenablog.com/entry/2019/07/15/120000
[hie-install-devcon-repo]: https://github.com/haskell/haskell-ide-engine#installation-with-ghc-and-hie-as-a-vs-code-devcontainer
[hie-install-devcon]: https://hmemcpy.com/2020/02/setting-up-a-haskell-development-environment-in-minutes-in-vscode/

### ビルド時間 (参考)

参考までに僕の環境 (Mac) 

- MacBook Air (Retina, 13-inch, 2018)
- CPU: 1.6 GHz Dual-Core Intel Core i5
- Memory: 16 GB 2133 MHz LPDDR3

で **v1.0.0.0** をビルドした場合は**22分**かかりました。(色々キャッシュとか効いてるはずなので、フルビルドの場合は1時間~2時間ぐらいかかるかもしれません。)

```shell
$ stack ./install.hs hie
...
Build completed in 22m31s
```

## Haskell の開発環境について

みなさん Haskell の開発環境ってどうしてますか？僕は [Vim and Haskell in 2016][soh-results] を参考にしつつ、 [haskell-vim-now][haskell-vim-now-repo] で環境を整えていました。これはこれで便利なのですが、やっぱり **IDE** を使いたいって思う時ありますよね。

**Haskell** 界隈でもこの話題は昔から問題として認識されていて、Haskell で書かれている [Leksah][leksah-official] (10年以上続いているプロジェクト) や [Haskell for Mac][haskell-for-mac-official] (有料ですが **SpriteKit** を使ってゲームを作れたりします。開発スピードは遅めです。**Xcode** の **playground** 機能も実装されています。個人的に購入してみましたが [stack][stack-official] と相性が悪いのですぐに使わなくなりました) などの IDE が存在します。

また、**IDE** を作るのではなく、バックエンドを実装するプロジェクトも存在します。

- [ghcide][ghcide-repo] (digital-asset 製)
- [ide-backend][ide-backend-repo] (fpco 製)
  - 最近は開発が止まっているようです。
- ~~[ghc-mod][ghc-mod-repo] (超有名)~~
  - [ツールとしてのghc-mod がとうとう deprecateに - reddit][ghc-mod-reddit]
- ~~[intero][intero-repo] (commercialhaskell 製)~~
  - [The intero project has reached it's end of life cycle -reddit][intero-reddit]

そして、**ghc-mod** と **ide-backend** が力を合わせてすごいやつ作るぞ！となって、絶賛開発中なのがこちら。

- [haskell-ide-engine][hie-repo] (略称は HIE)

将来的には、このプロジェクトに全て統合していくようです。

説明が長くなりましたが、[Summer of Haskell 2017]() に **Haskell IDE Engine** というプロジェクトがあり、以下を実装することを目標としていました。

- [Visual Studio Code][vscode-official] や [Emacs][emacs-official], [NeoVim][neovim-official] などの様々なエディタに対してインタフェースを提供するために、完全な [LSP (Language Server Protocol)][slp-official] をサポートする。
- 自動補完, 定義へのジャンプ, 型の挿入, 静的解析などの機能を実装するために [intero][intero-repo], [ghc-mod][ghc-mod-repo], [HaRe][hare-repo], [hindent][hindent-repo] などの既存の **Haskell** ツールを統合する。

今までは自分たちで [HLint][hlint-repo] や [HaRe][hare-repo] などのツールを別途インストールし、実行する必要がありましたが、HIE を使うことでこれらの作業負担が無くなる点も Good Point だと思います。([Tools in the ecosystem][hie-tools] を見てもらえればわかりますが、本当にたくさんあります。)

現在使える機能は以下の通りです。

- [HLint][hlint-repo], [ghc-mod][ghc-mod-repo] を使った静的解析
- [apply-refact][apply-refact-repo] を使ったクイックフィックス
- マウスホバー時の型情報の表示と [Hoogle][hoogle-repo] を使ったドキュメントの表示
- 定義へのジャンプ
- トップレベル定義の一覧表示
- ドキュメント内のハイライト参照
- 自動補完
- [Ormolu][ormolu-repo], [brittany][brittany-repo], [Floskell][floskell-repo] を使ったコード整形
- [HaRe][hare-repo] を使った名前変更リファクタリング
- import 文の自動挿入
- import 文からパッケージを `.cabal` ([Cabal][cabal-official]) ファイルや `hpack.yaml` ([hpack][hpack-repo]) に追加
- typo のクイックフィックス
- [hsimport][hsimport-repo] を使った import 文の挿入
- [LiquidHaskell][liquidhaskell-repo] のサポート
- Case splitting

**HIE** は **HLint**, **brittany**, **ghc-mod** 等の各パッケージが提供している **API** を利用して実装しているため、それぞれのバイナリファイルは必要ありません。(つまり、`stack install hlint` などでインストールしなくても使えるということです)

[soh-results]: https://summer.haskell.org/news/2017-09-15-final-results.html

[cabal-official]: https://www.haskell.org/cabal/
[emacs-official]: https://www.gnu.org/software/emacs/
[haskell-for-mac-official]: http://haskellformac.com/
[leksah-official]: http://leksah.org/
[neovim-official]: https://neovim.io/
[slp-official]: https://langserver.org/
[stack-official]: https://docs.haskellstack.org/en/stable/README/
[vscode-official]: https://code.visualstudio.com/

[apply-refact-repo]: https://github.com/mpickering/apply-refact
[brittany-repo]: https://github.com/lspitzner/brittany
[floskell-repo]: https://github.com/ennocramer/floskell
[ghc-mod-repo]: https://github.com/DanielG/ghc-mod
[ghc-mod-reddit]: https://www.reddit.com/r/haskell_jp/comments/ajxtlc/%E3%83%84%E3%83%BC%E3%83%AB%E3%81%A8%E3%81%97%E3%81%A6%E3%81%AEghcmod_%E3%81%8C%E3%81%A8%E3%81%86%E3%81%A8%E3%81%86_deprecate%E3%81%AB/
[ghcide-repo]: https://github.com/digital-asset/ghcide
[hare-repo]: https://github.com/RefactoringTools/HaRe
[haskell-vim-now-repo]: https://github.com/begriffs/haskell-vim-now
[hie-repo]: https://github.com/haskell/haskell-ide-engine
[hie-tools]: https://github.com/haskell/haskell-ide-engine/blob/master/docs/Tools.md
[hindent-repo]: https://github.com/commercialhaskell/hindent
[hlint-repo]: https://github.com/ndmitchell/hlint
[hoogle-repo]: https://github.com/ndmitchell/hoogle
[hpack-repo]: https://github.com/sol/hpack
[hsimport-repo]: https://github.com/fendor/hsimport
[ide-backend-repo]: https://github.com/fpco/ide-backend
[intero-repo]: https://github.com/commercialhaskell/intero
[intero-reddit]: https://www.reddit.com/r/haskell/comments/dr91dv/the_intero_project_has_reached_its_end_of_life/
[liquidhaskell-repo]: https://github.com/ucsd-progsys/liquidhaskell
[ormolu-repo]: https://github.com/tweag/ormolu

### その他の開発環境について

その他、参考になりそうな記事へのリンクです。

- [GHC 環境構築 概観 と PowerShell][hie-install-article-1]
- [HaskellやっていくGHC8.8.1令和元年白露の候][hie-install-article-2]
- [最近の自分の Haskell 開発環境（Windows）][hie-install-article-3]
- [Vim and Haskell in 2019][hie-install-article-4]

[hie-install-article-1]: https://kakkun61.hatenablog.com/entry/2019/11/20/GHC_%E7%92%B0%E5%A2%83%E6%A7%8B%E7%AF%89_%E6%A6%82%E8%A6%B3_%E3%81%A8_PowerShell
[hie-install-article-2]: http://syocy.hatenablog.com/entry/init-ghc-8-8-1
[hie-install-article-3]: https://kakkun61.hatenablog.com/entry/2019/10/23/%E6%9C%80%E8%BF%91%E3%81%AE%E8%87%AA%E5%88%86%E3%81%AE_Haskell_%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83%EF%BC%88Windows%EF%BC%89
[hie-install-article-4]: http://marco-lopes.com/articles/Vim-and-Haskell-in-2019/

## HIE のデメリット

良い部分だけ紹介するのは不公平なので、利用していて気になっている部分を箇条書きで列挙します。

- プロジェクトごとに異なる **HIE** のバイナリが必要なので、ビルドの時間が長いです。またかなりディスク容量を消費します ([Wild thought: use ghc-hotswap to load plugins #904][hie-issue-904] という取り組みも今後の野望として考えているようです)
- 良くも悪くも安定していないので、**master** ブランチが壊れている場合があります
- **macOS** で **TemplateHaskell** を利用しているファイルを開くと **HIE** が動かなくなることがあります
  - [Doesn't work with Template Haskell #1061][hie-issue-1061]
  - [TH exception #1084][hie-issue-1084]

上記の問題はあるものの、その問題点を確実に上回るメリットがあるため現在も利用を続けています。

- [type: bug][hie-issue-bug]
- [type: performance][hie-issue-performance]

[hie-issue-904]: https://github.com/haskell/haskell-ide-engine/issues/904
[hie-issue-1061]: https://github.com/haskell/haskell-ide-engine/issues/1061
[hie-issue-1084]: https://github.com/haskell/haskell-ide-engine/issues/1084
[hie-issue-bug]: https://github.com/haskell/haskell-ide-engine/issues?q=is%3Aissue+is%3Aopen+label%3A%22type%3A+bug%22
[hie-issue-performance]: https://github.com/haskell/haskell-ide-engine/issues?q=is%3Aissue+is%3Aopen+label%3A%22type%3A+performance%22

## 準備

### 依存関係のインストール

**git** はすでにインストールされているものとします。

```shell
# Debian 9/Ubuntu 18.04 or earlier
$ sudo apt install libicu-dev libtinfo-dev libgmp-dev zlib1g-dev

# Debian 10/Ubuntu 18.10 or later
$ sudo apt install libicu-dev libncurses-dev libgmp-dev zlib1g-dev
```

**zlib1g-dev** は[公式ドキュメント][hie-pre-requirements]に記載が無いため環境によっては不必要かもしれませんが、ハマりポイントっぽいのでインストールしておいた方が良いと思います。

[hie-pre-requirements]: https://github.com/haskell/haskell-ide-engine#linux-specific-pre-requirements

### Stack のインストール

```shell
$ curl -sSL https://get.haskellstack.org/ | sh
```

既に [stack][stack-official] をインストールしている人でバージョンが **2.1.1** より小さい場合は、以下のコマンドで更新しましょう。

```shell
$ stack upgrade

$ stack --version
Version 2.1.3, Git revision 0fa51b9925decd937e4a993ad90cb686f88fa282 (7739 commits) x86_64 hpack-0.31.2
```

### パスを通す

すでに設定している方は必要ありません。

だいたいこんな感じです。必要に応じて変更しましょう。

- **Mac** ユーザ

```shell
$ echo 'export PATH=$(stack path --local-bin):$PATH' >> ~/.bash_profile
$ source ~/.bash_profile
```

- **Ubuntu** ユーザ

```shell
$ echo 'export PATH=$(stack path --local-bin):$PATH' >> ~/.bashrc
$ source ~/.bashrc
```

## HIE が対応している GHC のバージョン

`install.hs` スクリプトの **help** コマンドで、利用可能なコマンドの一覧が表示されます。

利用可能な **GHC** のバージョン一覧もここで確認できます。

```shell
$ stack ./install.hs help
... GHC のインストールが始まる場合があります ...

Usage:
    stack install.hs <target> [options]
    or
    cabal v2-run install.hs --project-file install/shake.project -- <target> [options]

Targets:
    help                Show help message including all targets
                        
    hie                 Install hie with the latest available GHC and the data files
    latest              Install hie with the latest available GHC
    data                Get the required data-files for `hie` (Hoogle DB)
    hie-8.4.2           Install hie for GHC version 8.4.2
    hie-8.4.3           Install hie for GHC version 8.4.3
    hie-8.4.4           Install hie for GHC version 8.4.4
    hie-8.6.4           Install hie for GHC version 8.6.4
    hie-8.6.5           Install hie for GHC version 8.6.5
    hie-8.8.1           Install hie for GHC version 8.8.1
    hie-8.8.2           Install hie for GHC version 8.8.2
                        
    dev                 Install hie with the default stack.yaml
                        
    icu-macos-fix       Fixes icu related problems in MacOS

Options:
    -j[N], --jobs[=N]   Allow N jobs/threads at once [default number of CPUs].
    -s, --silent        Don't print anything.
    -q, --quiet         Print less (pass repeatedly for even less).
    -V, --verbose       Print more (pass repeatedly for even more).
```

## HIE のインストール

- [haskell/haskell-ide-engine][hie-repo]

**HIE** はビルドに利用した **GHC** のバージョンを利用するプロジェクトでしか動きません。(具体例: **GHC-8.6.5** でビルドした **HIE** は **GHC-8.8.3** を利用するプロジェクトでは使えません)

そのため、各 **GHC** のバージョンに対応した **HIE** のバイナリファイルを **hie-8.6.5** のようにリネームすることで、この問題を回避しています。

通常の良くある `stack install` コマンドではなく `install.hs` スクリプトを使ったインストール方法を推奨するのは、それらのつまらない作業を全部やってくれるからです。また、このスクリプトは [Shake][shake-official] というビルドシステムを利用しているため、OS に依存することなく実行可能です。そのため **Windows** でも **Linux** でも `install.hs` を使ってインストールを行います。

インストール方法には

1. 最新の **GHC** のバージョンに対応する **HIE** をインストールする方法
1. 指定したバージョンの **GHC** に対応する **HIE** をインストールする方法

の2パターンがあります。

[shake-official]: https://shakebuild.com/

### HIE をバージョンごとにインストールする方法

以下は全て **stack** を使ってインストールする方法になります。**cabal** を使いたい人は後述の方法を試してください。

2つ以上のバージョンを利用したい場合は、次に説明するインストール方法を単純に組み合わせるだけで大丈夫です。

#### 最新の GHC (8.8.2)

```shell
$ git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
$ cd haskell-ide-engine
$ stack ./install.hs hie
```

**hie** コマンドは最新版の **HIE** のインストールと **hoogle** ドキュメントの生成の両方を行います。

#### GHC 8.8.2

```shell
$ git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
$ cd haskell-ide-engine
$ stack ./install.hs hie-8.8.2
$ stack ./install.hs data
```

#### GHC 8.8.1

```shell
$ git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
$ cd haskell-ide-engine
$ stack ./install.hs hie-8.8.1
$ stack ./install.hs data
```

#### GHC 8.6.5

```shell
$ git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
$ cd haskell-ide-engine
$ stack ./install.hs hie-8.6.5
$ stack ./install.hs data
```

#### GHC 8.6.4

```shell
$ git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
$ cd haskell-ide-engine
$ stack ./install.hs hie-8.6.4
$ stack ./install.hs data
```

#### GHC 8.4.4

```shell
$ git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
$ cd haskell-ide-engine
$ stack ./install.hs hie-8.4.4
$ stack ./install.hs data
```

#### GHC 8.4.3

```shell
$ git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
$ cd haskell-ide-engine
$ stack ./install.hs hie-8.4.3
$ stack ./install.hs data
```

#### GHC 8.4.2

```shell
$ git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
$ cd haskell-ide-engine
$ stack ./install.hs hie-8.4.2
$ stack ./install.hs data
```

### stack ではなく cabal を使う

**HIE** のビルドに `stack build` ではなく `cabal build` を使うこともできます。**cabal** のバージョンは **2.4.1.0** 以降であれば利用可能です。

ただし、**Windows** 環境の場合は **3.0.0.0** 以降でしか動きません。

**cabal** を使う場合の基本形はこんな感じです。`cabal-hie-install` というスクリプト経由でコマンドを実行します。

```shell
$ ./cabal-hie-install help
... ビルドが実行される場合があります。

Usage:
    stack install.hs <target> [options]
    or
    cabal v2-run install.hs --project-file install/shake.project -- <target> [options]

Targets:
    help                Show help message including all targets
                        
    hie                 Install hie with the latest available GHC and the data files
    latest              Install hie with the latest available GHC
    data                Get the required data-files for `hie` (Hoogle DB)
    hie-8.4.4           Install hie for GHC version 8.4.4
    hie-8.6.4           Install hie for GHC version 8.6.4
    hie-8.6.5           Install hie for GHC version 8.6.5
    hie-8.8.1           Install hie for GHC version 8.8.1
    hie-8.8.2           Install hie for GHC version 8.8.2
                        
    ghcs                Show all GHC versions that can be installed via `cabal-build`.
                        
    icu-macos-fix       Fixes icu related problems in MacOS

Options:
    -j[N], --jobs[=N]   Allow N jobs/threads at once [default number of CPUs].
    -s, --silent        Don't print anything.
    -q, --quiet         Print less (pass repeatedly for even less).
    -V, --verbose       Print more (pass repeatedly for even more).
```

利用可能なGHCの一覧は `cabal-hie-install ghcs` コマンドで確認できます。

```shell
$ ./cabal-hie-install ghcs
********************************************************************************
Found the following GHC paths:
ghc-8.4.4: ~/.ghcup/bin/ghc-8.4.4
ghc-8.6.4: ~/.ghcup/bin/ghc-8.6.4
ghc-8.6.5: ~/.ghcup/bin/ghc-8.6.5
ghc-8.8.1: ~/.ghcup/bin/ghc-8.8.1
ghc-8.8.2: ~/.ghcup/bin/ghc-8.8.2

********************************************************************************
```

例として、**GHC-8.8.2** 対応の **HIE** をビルドする場合は以下のようになります。

```shell
$ cabal update
$ ./cabal-hie-install hie-8.8.2
$ ./cabal-hie-install data
```

## VSCODE の設定

無事にインストールが完了し、パスが通っている状態であればこのように **HIE** のバージョンが表示されます。

```shell
$ hie --version
Version 1.2 x86_64 ghc-8.8.2
```

## トラブルシューティング

本家の [Troubleshooting][hie-repo-troubleshooting] も参考になる場合があります。

[hie-repo-troubleshooting]: https://github.com/haskell/haskell-ide-engine#troubleshooting

---

### hie doesn't work with error "cannot satisfy -package-id foo ... "

- [hie doesn't work with error "cannot satisfy -package-id foo ... " on any Linux #1616][hie-issue-1616]

上記の **issue** 通り、プロジェクトのルートで以下のコマンドを実行すれば直ります。

```shell
$ echo "cradle: { stack: {}}" > hie.yaml
$ stack build
```

`stack build` だけで直る場合もあります。

[hie-issue-1616]: https://github.com/haskell/haskell-ide-engine/issues/1616

---

### Mismatching GHC versions: GHC session is No System GHC Found., HIE is 8.6.5 You may want to use hie-wrapper.Check the README for more information

- [Warning "Mismatching GHC versions" in VS Code][hie-issue-1658]

**vscode** でフォルダを開く際に、プロジェクトルートを開いているか確認してみてください。

例えば、ホームディレクトリなど、プロジェクトルートではない場所で作業している場合に発生します。

[hie-issue-1658]: https://github.com/haskell/haskell-ide-engine/issues/1658

---

### hie が見つからない

![vscode のポップアップ](/images/hie/vscode/not-found-hie.png)

> hie executable missing, please make sure it is installed, see github.com/haskell/haskell-ide-engine.

画像のようなエラーメッセージが出た時は **hie** が見つからないということなので、以下のように実行ファイルのパスを `languageServerHaskell.hieExecutablePath` に指定します。

```json
"languageServerHaskell.hieExecutablePath": "~/.local/bin/hie",
```

どこにインストールしたかわからない人は次のコマンドでパスを確認しましょう。

```shell
$ which hie
~/.local/bin/hie
```

---

### "print-build-platform" (exit 1): failed というエラーが出る

エラーメッセージは以下のような内容が表示されます。

```shell
readCreateProcess: <path>/bin/cabal-helper-wrapper "print-build-platform" (exit 1): failed
```

このエラーは [cabal-helper-wrapper "print-build-platform" (exit 1): failed. #128][vscode-hie-server-issue-128] の可能性があります。

下記の内容を `.bash_profile` 等に追記すると解決する場合があります。

```bash
export PATH=$(stack path --compiler-bin):$PATH
```

[vscode-hie-server-issue-128]: https://github.com/alanz/vscode-hie-server/issues/128

---

### Error: spawn EACCES というエラーが出て HIE が起動しない

```shell
[Error - 19:07:38] Starting client failed
Error: spawn EACCES
	at _errnoException (util.js:1024:11)
	at ChildProcess.spawn (internal/child_process.js:323:11)
	at Object.exports.spawn (child_process.js:514:9)
	at _getServerWorkingDir.then.serverWorkingDir (/home/guchi/.vscode/extensions/alanz.vscode-hie-server-0.0.24/node_modules/vscode-languageclient/lib/main.js:347:40)
	at <anonymous>
```

上記のエラーは、パスの指定が間違っている場合などで出現するようです。

例えば、以下のパスはどちらも間違っています。自分の設定ファイルを確認してみましょう。

- 間違った指定その１

```json
"languageServerHaskell.hieExecutablePath": "~/.local/bin/",
```

- 間違った指定その2

```json
"languageServerHaskell.hieExecutablePath": "${HOME}/.local/bin/hie",
```

以下のように、正しいパスを指定しましょう。

```json
"languageServerHaskell.hieExecutablePath": "~/.local/bin/hie",
```

---

### text-icu パッケージのビルドエラー

**macOS** 環境だと、以下のようなエラーが出る場合があります。

```shell
--  While building custom Setup.hs for package text-icu-0.7.0.1 using:
      /Users/bm12/.stack/setup-exe-cache/x86_64-osx/Cabal-simple_mPHDZzAJ_1.24.2.0_ghc-8.0.2 --builddir=.stack-work/dist/x86_64-osx/Cabal-1.24.2.0 configure --with-ghc=/Users/bm12/.stack/programs/x86_64-osx/ghc-8.0.2/bin/ghc --with-ghc-pkg=/Users/bm12/.stack/programs/x86_64-osx/ghc-8.0.2/bin/ghc-pkg --user --package-db=clear --package-db=global --package-db=/Users/bm12/.stack/snapshots/x86_64-osx/lts-9.21/8.0.2/pkgdb --libdir=/Users/bm12/.stack/snapshots/x86_64-osx/lts-9.21/8.0.2/lib --bindir=/Users/bm12/.stack/snapshots/x86_64-osx/lts-9.21/8.0.2/bin --datadir=/Users/bm12/.stack/snapshots/x86_64-osx/lts-9.21/8.0.2/share --libexecdir=/Users/bm12/.stack/snapshots/x86_64-osx/lts-9.21/8.0.2/libexec --sysconfdir=/Users/bm12/.stack/snapshots/x86_64-osx/lts-9.21/8.0.2/etc --docdir=/Users/bm12/.stack/snapshots/x86_64-osx/lts-9.21/8.0.2/doc/text-icu-0.7.0.1 --htmldir=/Users/bm12/.stack/snapshots/x86_64-osx/lts-9.21/8.0.2/doc/text-icu-0.7.0.1 --haddockdir=/Users/bm12/.stack/snapshots/x86_64-osx/lts-9.21/8.0.2/doc/text-icu-0.7.0.1 --dependency=base=base-4.9.1.0 --dependency=bytestring=bytestring-0.10.8.1 --dependency=deepseq=deepseq-1.4.2.0 --dependency=text=text-1.2.2.2-9UQZjEJZQFSGMffj1Z5g00
    Process exited with code: ExitFailure 1
    Logs have been written to: /usr/local/share/haskell-ide-engine/.stack-work/logs/text-icu-0.7.0.1.log

    Configuring text-icu-0.7.0.1...
    Cabal-simple_mPHDZzAJ_1.24.2.0_ghc-8.0.2: Missing dependencies on foreign
    libraries:
    * Missing C libraries: icuuc, icui18n, icudata
    This problem can usually be solved by installing the system packages that
    provide these libraries (you may need the "-dev" versions). If the libraries
    are already installed but in a non-standard location then you can use the
    flags --extra-include-dirs= and --extra-lib-dirs= to specify where they are.
make: *** [build] Error 1
```

依存しいてるライブラリのパスがわからなくてパッケージのインストールに失敗しているため、以下のコマンドでパッケージをビルドしましょう。

```shell
$ stack ./install.hs icu-macos-fix
```

## VS Code のインストール

### Ubuntu 18.04 LTS

[公式ドキュメント][vscode-official-installation]で紹介されている、手動でのインストール方法でインストールすることにします。

```shell
$ curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
$ sudo install -o root -g root -m 644 packages.microsoft.gpg /usr/share/keyrings/
$ sudo sh -c 'echo "deb [arch=amd64 signed-by=/usr/share/keyrings/packages.microsoft.gpg] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'

$ sudo apt install apt-transport-https
$ sudo apt update
$ sudo apt install code # or code-insiders
```

`code` と入力すればすぐに *vscode* が起動します。

```shell
$ code
```

![vscode 起動時の画面](/images/hie/vscode/vscode-welcome.png)

以下のようなエラーが出る場合はライブラリが足りないので、追加でインストールしましょう。

```shell
$ code
/usr/share/code/bin/../code: error while loading shared libraries: libXss.so.1: cannot open shared object file: No such file or directory

$ sudo apt install libxss1
```

[vscode-official-installation]: https://code.visualstudio.com/docs/setup/linux#_installation

### macOS Catalina (10.15.3)

[公式サイト][vscode-binary]からバイナリが配布されているので、それをインストールするだけです。

[vscode-binary]: https://code.visualstudio.com/

## 拡張機能 (Haskell Language Server) の追加

[VS Code marketplace][vscode-marketplace] から [Haskell Language Server][hie-lang-server-ext] 拡張をインストールします。

![マーケットプレイスから Haskell Language Server 拡張をインストール](/images/hie/vscode/haskell-language-server-extension-install.png)

インストール完了後、再読み込みすると有効化されます。

また、**vscode** 内からインストールすることも可能です。

![VSCode 内から Haskell Language Server 拡張をインストール](/images/hie/vscode/haskell-language-server-extension-install2.png)

**Haskell Language Server** をインストールすると **Haskell Syntax Highlighting** 拡張も同時にインストールされるため、何もしなくても色鮮やかなコードが表示されます。

![シンタックスハイライト](/images/hie/vscode/syntax-highlighting.png)

[vscode-marketplace]: https://marketplace.visualstudio.com/
[hie-lang-server-ext]: https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server

### 設定可能な項目

名前 | 型 | デフォルト値 | 備考
-----|-----|----|-----------
hlintOn | boolean | true
maxNumberOfProblems | number | 100
diagnosticsOnChange | boolean | true
liquidOn | boolean | false
completionSnippetsOn | boolean | true
formatOnImportOn | boolean | true
formattingProvider | string | "brittany" | Enum: ["brittany", "floskell", "ormolu", "none"]
hieExecutablePath | string | ""
useCustomHieWrapper | boolean | false
useCustomHieWrapperPath | string | ""
noLspParam | boolean | false
showTypeForSelection.onHover | boolean | true
showTypeForSelection.command.location | string | dropdown | Enum: ["dropdown", "channel"]
trace.server | string | "off" | Enum: ["off", "messages", "verbose"]
logFile | string | ""
enableHIE | boolean | true

実際の設定は [vscode-hie-server/package.json][vscode-hie-server-config] で確認できます。

[vscode-hie-server-config]: https://github.com/alanz/vscode-hie-server/blob/master/package.json

### 利用可能なコマンド

コマンド名 | 説明 
---------|----------
demoteDef | Move a definition one level down
liftOneLevel | Move definition one level up from where it is now
liftTopLevel | Move a definition to the top level
genApplicative | Generalize a monadic function to use applicative
deleteDef | Deletes a definition
insertType | Insert type for the expression
showType | Show type for the expression
caseSplit | Generate pattern matches for the identifier under the cursor
importIdentifier | Imports a function or type based on a Hoogle search
restartHie | Restart the Hie LSP server

実際のコマンドは [vscode-hie-server/package.json][vscode-hie-server-config] で確認できます。

## 機能について

ここが一番重要ですね！まだまだ開発途中なので出来ることは限られているのですが、それでも凄く便利です。ここから利用するスクリーションは本家リポジトリで紹介されている画像になります。

### cabal, hpack へのパッケージ追加

![パッケージの追加](https://user-images.githubusercontent.com/2488460/43036067-20ae5964-8cf2-11e8-9951-4fd849b3f735.gif)

![パッケージの追加](https://user-images.githubusercontent.com/1387653/40287051-b6f987fe-5c5f-11e8-980f-ed7bfa1b2aec.gif)

### typo quick fixes

![typo quick fixes](https://user-images.githubusercontent.com/2488460/43036093-746ae176-8cf2-11e8-8b2d-59799b21c283.gif)

### 不足している import の追加 (via hsimport)

![Add missing imports](https://user-images.githubusercontent.com/2488460/43036113-9bb5d5b0-8cf2-11e8-8e32-20952378cf2b.gif)

### import 宣言の自動挿入

![import](https://user-images.githubusercontent.com/1387653/40287051-b6f987fe-5c5f-11e8-980f-ed7bfa1b2aec.gif)

このコマンドは **package.yaml** の依存関係から、選択した関数が含まれるモジュールをリストアップしてくれるので、その中から適切な物を選ぶと、自動的に挿入されます。

### Lint

**PROBLEMS (問題)** のエリアには **HLint**, **ghc-mod** のエラーや警告が表示されます。クリックすると、該当する行までジャンプするので便利です。

![lint](https://camo.githubusercontent.com/dcc0d19d6ddee2d7a5aa3e640d8517800871f990/687474703a2f2f692e696d6775722e636f6d2f3176716d3465462e676966)

### コードアクションとクイックフィックス

**HLint** に指摘されると行の左側に電球のマークが表示されます。これをクリックして適用するだけで、**HLint** の指摘通りにコードが書き換わります。

![quickfix](https://camo.githubusercontent.com/1f81802a108f8a96320e7be75f47924de29afe7a/687474703a2f2f692e696d6775722e636f6d2f644272536935462e676966)

失敗することもあるので、あまり使いません。

### マウスホバー時の型情報とドキュメント表示

マウスホバー時に型の情報とドキュメントが表示されます。ドキュメントの表示は **hoogle** を利用しています。

![hoogle](https://camo.githubusercontent.com/fb828845a665d6bff30340ba61bc9744013773a7/687474703a2f2f692e696d6775722e636f6d2f41637659524f762e676966)

#### ドキュメントが表示されない場合

以下の画像のように、画面上部に **No hoogle db found. Check the README for instructions to generate one** と表示されている場合はデータベースと上手く連携できていない状態です。

![No hoogle db found. Check the README for instructions to generate one](https://qiita-image-store.s3.amazonaws.com/0/46047/d20edcd0-16f7-b38b-7ca2-a483f861250a.png)

僕の環境では以下のバージョンでエラーとなってしまったので、 最新のLTSで **hoogle** をインストールし直しました。

```shell
$ hoogle --version
Hoogle 5.0.9, http://hoogle.haskell.org/

$ stack install hoogle --resolver lts
...

$ hoogle --version
Hoogle 5.0.17.15, https://hoogle.haskell.org/
```

#### プロジェクト固有のデータベースを利用する

**stack** のプロジェクトルートで通常通り **hoogle** のデータベースを生成すればOKです。

```shell
$ stack haddock --keep-going
```

**cabal** プロジェクトの場合は `~/.cabal/config` に以下の設定を追記します。

```
documentation: True
```

### 定義へのジャンプ

![定義ジャンプ](https://camo.githubusercontent.com/1ade7632d9ddfca985cebe6e58dfb2de55298bfe/687474703a2f2f692e696d6775722e636f6d2f6b6d435532427a2e676966)

### 全てのトップレベル定義の一覧表示

`Ctrl + P` でコマンドパレットを開き **\@** を入力することでトップレベルに定義された関数の一覧を確認できます。クリックすると、その関数の定義にジャンプします。

![トップレベル定義の一覧表示](https://camo.githubusercontent.com/c4a5943c4fd6effbecb731bc72446f6c37240ac3/687474703a2f2f692e696d6775722e636f6d2f474572635971702e676966)

### ソースコード内のハイライト参照

マウスでドラッグするだけで、全てハイライトされます。

![ハイライト参照](https://camo.githubusercontent.com/9579d2d31032396d24c148972aa07ab9b0007767/687474703a2f2f692e696d6775722e636f6d2f594c6a487332732e676966)

### 自動補完

文字を入力すると自動的に補完機能が働きます。タブキーで補完完了です。

![自動補完](https://camo.githubusercontent.com/c4c25789a4fb77d4e88ada6c179a5d72a8c13665/687474703a2f2f692e696d6775722e636f6d2f775236494a374d2e676966)

### コード整形

以下のフォーマッターが利用できます。

- [brittany][brittany-repo]
- [floskell][floskell-repo]
- [ormolu][ormolu-repo]

![フォーマッターを適用](https://camo.githubusercontent.com/03cebefc68034e3f1bf75350c2febaf93128d86d/687474703a2f2f692e696d6775722e636f6d2f63715a5a3848432e676966)

### 名前の変更

**HaRe** を使って名前変更のリファクタリングができます。

![HaReを使ったリファクタリング](https://camo.githubusercontent.com/db74493d7eafa915818cde11d7457161c827c0c0/687474703a2f2f692e696d6775722e636f6d2f7a3033473261352e676966)

## その他の拡張機能について

**Haskell** に関する拡張機能はそれほど多くはありませんが、[Haskell GHCi debug viewer Phoityne][ghci-debug-phoityne-ext] などは [GHCi debugger][ghci-debugger-doc] が使えるようになるという面白い拡張です。

一応こんな感じで動きます。

![DEMO](/images/hie/vscode/phoityne-vscode.gif)

[ghci-debug-phoityne-ext]: https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode
[ghci-debugger-doc]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-debugger

## HIE がサポートしているエディタ

**HIE** は **vscode** に限らず **Language Server Protocol** が利用できれば、どんなエディタでも利用できます。公式の **README** で明記があるのは以下のエディタです。

- [VS Code][hie-editor-vscode]
- [Sublime Text][hie-editor-sublime]
- [Vim, Neovim][hie-editor-vim]
- [Atom][hie-editor-atom]
- [Emacs][hie-editor-emacs]
  - [Emacs で Haskell IDE Engine を使う][hie-editor-emacs-bigmoon]
- [Spacemacs][hie-editor-spacemacs]
- [Oni][hie-editor-oni]

[hie-editor-vscode]: https://github.com/haskell/haskell-ide-engine#using-hie-with-vs-code
[hie-editor-sublime]: https://github.com/haskell/haskell-ide-engine#using-hie-with-sublime-text
[hie-editor-vim]: https://github.com/haskell/haskell-ide-engine#using-hie-with-vim-or-neovim
[hie-editor-atom]: https://github.com/haskell/haskell-ide-engine#using-hie-with-atom
[hie-editor-emacs]: https://github.com/haskell/haskell-ide-engine#using-hie-with-emacs
[hie-editor-emacs-bigmoon]: https://haskell.e-bigmoon.com/hie/emacs.html
[hie-editor-spacemacs]: https://github.com/haskell/haskell-ide-engine#using-hie-with-spacemacs
[hie-editor-oni]: https://github.com/haskell/haskell-ide-engine#using-hie-with-oni

## まとめ

**vscode** 内の統合ターミナルウィンドウで以下のコマンドを一度入力しておけば、保存時に自動的にリビルドしてくれるのでオススメです。

```shell
$ stack test --fast --file-watch --no-run-tests

# コンパイルの遅さが気になる人はこちらのコマンドが良いです
$ stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -qg -RTS"
```

最新の **IDE** 環境でモダンな **Haskell** アプリケーション開発を楽しみましょう！
