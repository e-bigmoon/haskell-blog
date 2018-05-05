---
title: プログラムの実行
date: 2018/05/05
prev: ./create-lib.html
next: ./create-app.html
---

## ghci を使ってプログラムのデバッグを行う

ここまでの内容を対話環境で実行したいと思います。

```shell
$ stack repl
PFAD-0.1.0.0: configure (lib + exe)
Configuring PFAD-0.1.0.0...
PFAD-0.1.0.0: initial-build-steps (lib + exe)
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: PFAD
Using main module: 1. Package `PFAD' component exe:PFAD-exe with main-is file: /Users/bm12/Desktop/PFAD/app/Main.hs
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Main             ( /Users/bm12/Desktop/PFAD/app/Main.hs, interpreted )
[2 of 2] Compiling Minfree          ( /Users/bm12/Desktop/PFAD/src/Minfree.hs, interpreted )

/Users/bm12/Desktop/PFAD/src/Minfree.hs:3:1: error:
    Could not find module ‘Data.Array’
    Perhaps you meant Data.Proxy (from base-4.10.1.0)
    Use -v to see a list of the files searched for.
  |
3 | import Data.Array (Array, elems, accumArray, assocs)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

/Users/bm12/Desktop/PFAD/src/Minfree.hs:4:1: error:
    Could not find module ‘Data.Array.ST’
    Use -v to see a list of the files searched for.
  |
4 | import Data.Array.ST (runSTArray, newArray, writeArray)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Failed, one module loaded.

<no location info>: error:
    Could not find module ‘Minfree’
    It is not a module in the current program, or in any known package.
Loaded GHCi configuration from /private/var/folders/z2/kqfdprn54qn582gkscbfncq80000gn/T/haskell-stack-ghci/59ba0db0/ghci-script
```

残念ながらコンパイルエラーになってしまいました。

エラーメッセージを確認しつつ、エラーを修正していきましょう！

### エラーメッセージの読み方

```shell
/Users/bm12/Desktop/PFAD/src/Minfree.hs:3:1: error:
    Could not find module ‘Data.Array’
```

このエラーメッセージから以下のことがわかります。

- **/Users/bm12/Desktop/PFAD/src/Minfree.hs** ファイルでエラーが起きている
- 上記ファイルの **3行目** の **1列目** 付近があやしい
- **Data.Array** というモジュールが見つからなかった

逆に、続く以下のメッセージは初心者殺しだと思います・・・。

```shell
Perhaps you meant Data.Proxy (from base-4.10.1.0)
```

これは **Data.Array** モジュールが見つからなかったため、**GHC** は **もしかしたら** 入力ミスなんじゃないか？と親切に考えてくれます。

つまり、**GHC** が見つけられる範囲で **Data.Array** に一番近いモジュール名を提示してくれます。

**package.yaml** で **base** パッケージを指定しているため **GHC** は **base** パッケージに含まれているモジュール名から検索を行い、一番近い **Data.Proxy** ではないですか？とメッセージを表示してくれています。

このメッセージどおり修正しても別のコンパイルエラーになるだけです。

今回のエラーの原因は **Data.Array** が **base** パッケージに存在しないことに起因しているため、正しい解決策としては **Data.Array** を含むパッケージを依存関係に追加する。ということになります。

## ghci でよく使うコマンド

**ghci** の対話セッションから抜ける際は **:q** と入力します。

## GHC について

**ghci** は **Glasgow Haskell Compiler’s interactive environment** の意味です。 **GHC** はイギリスのグラスゴー大学で開発された **Haskell** コンパイラなのでこのような名前がついています。注意して欲しい点としては **Haskell == GHC** と理解してしまうことです。

**Haskell** というプログラミング言語の仕様が **Haskell98**, **Haskell2010** と続いてきたなかで実際にこの仕様に沿ったプログラムを実行できるコンパイラ (処理系) の一つが **GHC** というのが正しい理解です。他にも **UHC** や **LHc** などがあるそうです。ですので、**Haskell2010** の仕様に無い機能等は **GHC拡張** として実装されています ([Implementations](https://wiki.haskell.org/Implementations))。

処理系が複数あると言っても主流 (デファクトスタンダード) は **GHC** です。なので、基本的に **Haskell** の話をする時はみんな頭の中に **GHC** がインストールされていると思いますし、実際の開発において **GHC** 以外を採用することは一般的に考えにくいです。

## 依存関係の追加

**base** パッケージに含まれていないモジュールを **import** して使いたいというのは実際の開発においても頻繁に起こります。

モジュール名からパッケージ名を検索するためには [Hayoo!](http://hayoo.fh-wedel.de/) や [Hoogle](https://www.haskell.org/hoogle/) を使うと便利です。

実際に[Hayoo! で検索](http://hayoo.fh-wedel.de/?query=Data.Array)してみると [array](https://www.stackage.org/haddock/lts-11.7/array-0.5.2.0/Data-Array.html) パッケージに含まれていそうだということがわかります。

**package.yaml** ファイルの **dependencies** に **array** を追記します。

```package.yaml
dependencies:
- base >= 4.7 && < 5
- array
```

そしてプロジェクトをビルドするだけで、**stack** は自動的に **array** パッケージ及び、その依存関係をインストールしてくれます。

では、もう一度対話環境で読み込んでみましょう。

今度はコンパイルエラーにならず **minfree** 関数と **minfree'** 関数が利用できるようになっているはずです。

```shell
$ stack repl
...

*Main Minfree> minfree [0,1,3,4]
2

*Main Minfree> minfree’ [0,1,3,4]
2
```