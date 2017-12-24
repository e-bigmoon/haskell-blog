---
title: プログラムの実行
---

ここまでの内容を対話環境で実行したいと思います。

```shell-session
$ stack ghci
PFAD-0.1.0.0: configure (lib + exe)
Configuring PFAD-0.1.0.0...
PFAD-0.1.0.0: initial-build-steps (lib + exe)
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: PFAD
Using main module: 1. Package `PFAD' component exe:PFAD-exe with main-is file: /home/bm12/Desktop/PFAD/app/Main.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Minfree          ( /home/bm12/Desktop/PFAD/src/Minfree.hs, interpreted )

/home/bm12/Desktop/PFAD/src/Minfree.hs:3:1: error:
    Failed to load interface for ‘Data.Array’
    It is a member of the hidden package ‘array-0.5.1.1’.
    Use -v to see a list of the files searched for.

/home/bm12/Desktop/PFAD/src/Minfree.hs:4:1: error:
    Failed to load interface for ‘Data.Array.ST’
    It is a member of the hidden package ‘array-0.5.1.1’.
    Use -v to see a list of the files searched for.
Failed, modules loaded: none.

<no location info>: error:
    Could not find module ‘Minfree’
    It is not a module in the current program, or in any known package.
Loaded GHCi configuration from /tmp/ghci308/ghci-script
```

このエラーメッセージでは、`Data.Array` と `Data.Array.ST` がインポートできていないため、エラーとなってしまったことを教えてくれています。また、`array-0.5.1.1` を利用したら良いこともこのエラーメッセージから読み取ることができます。

`ghci` の対話セッションから抜ける際は `:q` と入力します。

##### GHC について

`ghci` は `Glasgow Haskell Compiler’s interactive environment` の意味です。 `GHC` はイギリスのグラスゴー大学で開発された `Haskell` コンパイラなのでこのような名前がついています。注意して欲しい点としては `Haskell == GHC` と理解してしまうことです。

`Haskell` というプログラミング言語の仕様が `Haskell98`, `Haskell2010` と続いてきたなかで実際にこの仕様に沿ったプログラムを実行できるコンパイラ (処理系) の一つが `GHC` というのが正しい理解です。他にも `UHC` や `LHc` などがあるそうです。ですので、`Haskell2010` の仕様に無い機能等は `GHC拡張` として実装されています。=> [Implementations](https://wiki.haskell.org/Implementations)

処理系が複数あると言っても主流 (デファクトスタンダード) は `GHC` です。なので、基本的に `Haskell` の話をする時はみんな頭の中に `GHC` がインストールされていると思いますし、実務でも `GHC` 以外を採用することは一般的に考えにくいです。

##### 依存関係の追加
`base` パッケージに含まれていないモジュールを `import` して使いたいというのは実際の開発においても頻繁に起こります。

今回の場合は先ほどのエラーメッセージから `array` パッケージをインストールすれば良いことがわかっています。

`package.yaml` ファイルの `library` に `dependencies` を追記します。

```yaml:package.yaml
library:
  source-dirs: src
  dependencies:
  - array
```

そして次のコマンドでプロジェクトをビルドするだけで、`stack` は自動的に `array` パッケージ及び、その依存関係をインストールしてくれます。

```shell-session
$ stack build
```

##### GHC
###### Glasgow Haskell Compiler User's Guide
- [8. Profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html)