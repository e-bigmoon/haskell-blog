---
title: cabal の build-tool-depends フィールド
author: Shinya Yamaguchi
tags: bigmoon, cabal
---

cabal の [build-tool-depends](https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-build-tool-depends) というフィールドが便利だったので紹介します。

テストを書くときに [hspec-discover](https://hackage.haskell.org/package/hspec-discover) や [tasty-discover](https://hackage.haskell.org/package/tasty-discover) などを使う場合、このフィールドを設定しておくことで依存している実行ファイルを自動的にダウンロードして使ってくれます。

```shell
$ cabal --version
cabal-install version 3.0.0.0
compiled using version 3.0.0.0 of the Cabal library 
```

<!--more-->

## サンプルプロジェクトの構成

全体のディレクトリ構成は以下のようになっているとしましょう。

```shell
$ tree .
.
├── example.cabal
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    ├── LibSpec.hs
    └── Spec.hs
```

それぞれのファイルの内容は以下のようになっています。

```haskell
-- src/Lib.hs
module Lib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

```haskell
-- test/LibSpec.hs
module LibSpec (spec) where

spec :: Spec
spec = undefined
```

```haskell
-- test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

```cabal
-- example.cabal
cabal-version: 2.4
name:          example
version:       0.1.0.0

library
  exposed-modules:  Lib
  hs-source-dirs:   src
  build-depends:    base >=4.7 && <5
  default-language: Haskell2010

test-suite example-test
  type:             exitcode-stdio-1.0
  main-is:          Spec.hs
  hs-source-dirs:   test
  other-modules:    LibSpec
  build-depends:
    , base     >=4.7 && <5
    , example

  default-language: Haskell2010
```

```yaml
# stack.yaml
resolver: lts-14.20
packages:
- .
```

このような状況で `cabal` や `stack` がどのように振舞うか見ていきましょう。

現時点では `hspec-discover` の実行ファイルは存在していません。

```shell
$ hspec-discover
bash: hspec-discover: コマンドが見つかりません
```

## stack

`hspec-discover` の実行ファイルが無い状態で `stack test` を実行すると以下のようにエラーになります。

```shell
$ stack test
...

ghc: could not execute: hspec-discover
```

### hspec-discover をインストールした場合

```shell
$ stack install hspec-discover

$ hspec-discover
Usage: hspec-discover SRC CUR DST [--module-name=NAME]
```

上記のようにインストールした場合は、問題無く動作します。

```shell
$ stack test
...

example/test/LibSpec.hs:3:9: error:
    Not in scope: type constructor or class ‘Spec’
  |         
3 | spec :: Spec
  |         ^^^^
```

`hspec-discover` がファイルを自動的に見つけてくれたので、コンパイルエラーになりました。

次に進む前に実行ファイルを削除しておきます。

```shell
$ rm ~/.local/bin/hspec-discover
```

### build-depends に追加した場合

`example.cabal` ファイルの `build-depends` に追加した場合はどうなるのでしょうか？

```cabal
test-suite example-test
  type:             exitcode-stdio-1.0
  main-is:          Spec.hs
  hs-source-dirs:   test
  other-modules:    LibSpec
  build-depends:
    , base     >=4.7 && <5
    , example
    , hspec-discover  -- 新しく追加した

  default-language: Haskell2010
```

では `stack test` を実行してみます。

```shell
$ stack test
...

example/test/LibSpec.hs:3:9: error:
    Not in scope: type constructor or class ‘Spec’
  |         
3 | spec :: Spec
  |         ^^^^
```

実行ファイルがローカル環境に存在しなくても自動的に `hspec-discover` が動作しています。

そのため、`stack` プロジェクトでは `hspec-discover` を依存関係 (`build-depends`) に追加するだけで良い感じにテストが進みます。

## cabal

同様に `cabal` の場合も確かめてみましょう。(`example.cabal` ファイルから `hspec-discover` を削除しておきます)

```shell
$ cabal test
...

ghc: could not execute: hspec-discover
```

`stack` と同様のエラーになりました。

### hspec-discover をインストールした場合

```shell
$ cabal install hspec-discover

$ hspec-discover
Usage: hspec-discover SRC CUR DST [--module-name=NAME]
```

同様に `cabal test` を実行してみます。

```shell
$ cabal test
...

test/LibSpec.hs:3:9: error:
    Not in scope: type constructor or class ‘Spec’
  |
3 | spec :: Spec
  | 
```

`stack` の場合と同じように、ちゃんと動いていますね。

### build-depends に追加した場合

まずはバイナリファイルを削除しておきます。

```shell
$ rm ~/.cabal/bin/hspec-discover
```

`example.cabal` ファイルは先ほどと同じように `build-depends` に `hspec-discover` を追加した状態です。

テストを実行してみましょう。

```shell
$ cabal test
...

ghc: could not execute: hspec-discover
```

`stack` と異なる結果になりましたね・・・。僕は今までこの結果への対処法は `hspec-discover` をインストールしておくしかないと思っていました。

しかし、`build-tool-depends` をフィールドを利用することで `stack` と同じ挙動になるということがわかりました。

実際に試してみましょう。

### build-tool-depends に追加した場合

`build-tool-depends` に追加するとこんな感じです。

```cabal
test-suite example-test
  type:             exitcode-stdio-1.0
  main-is:          Spec.hs
  hs-source-dirs:   test
  other-modules:    LibSpec
  build-depends:
    , base     >=4.7 && <5
    , example

  build-tool-depends:                 -- 新たに追加した行
    , hspec-discover:hspec-discover   -- 新たに追加した行

  default-language: Haskell2010
```

`build-depends` が `<package_name>` という指定方法でしたが、`build-tool-depends` では `<package_name>:<exe_name>` という指定方法になっている点に注意です。

バージョン制約が必要な場合は `build-depends` と同じように指定できます。

```cabal
hspec-discover:hspec-discover ^>=2.7
```

これでテストを実行してみましょう。

```shell
$ cabal test
...

test/LibSpec.hs:3:9: error:
    Not in scope: type constructor or class ‘Spec’
  |
3 | spec :: Spec
  | 
```

これで `stack` と同じような挙動になりました！happy!

## まとめ

こんな便利なフィールドがあるなんて全然知りませんでした・・・。

- `build-tool-depends` を指定しておけば、実行ファイルが無くても大丈夫
- `stack` は初心者に優しい

[GHC-8.8.2 がリリース](https://www.haskell.org/ghc/blog/20200116-ghc-8.8.2-released.html)されましたね。

## 参考リソース

- [cabal user guide](https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-build-tool-depends)
