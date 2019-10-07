---
title: cabal-fmt の紹介
author: Shinya Yamaguchi
tags: bigmoon, tool
---

## はじめに

[cabal-fmt](https://hackage.haskell.org/package/cabal-fmt) でできること。

- **cabal** ファイルの整形
- フォルダを指定して自動でモジュールを expand  できる
- 以下のフィールドをアルファベット順で自動的に並べ替え&重複を削除
  - **exposed-modules**
  - **other-modules**
  - **default-extensions**
  - **other-extensions**
  - **build-depends**

```shell
λ cabal-fmt --version
0.1.1.1

λ cabal -V
cabal-install version 3.0.0.0
compiled using version 3.0.0.0 of the Cabal library

# 使い方
λ cabal-fmt <proj>.cabal -i
```

作者の記事: [ANN: cabal-fmt](http://oleg.fi/gists/posts/2019-08-11-cabal-fmt.html)

<!--more-->

## 具体例

### 適用前

```cabal
cabal-version: 2.4
name:test
version:0.1.0.0
tested-with:
  GHC ==8.8.1 || ==8.6.5 || ==8.4.4 || ==8.2.2 || ==8.0.2 || ==7.10.3
  GHCJS ==8.4

executable site
  main-is:          site.hs
  other-modules:
    Config
    Hakyll.Ext
  hs-source-dirs:   app
  ghc-options:
    -Wcompat -Wall
    -Wnoncanonical-monad-instances
    -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wtabs -threaded
  build-depends:
      base, lens
    , blaze-html, conduit-combinators
    , containers      , filepath
          , bytestring
    , extensible
    
    
    , yaml, time

  if !os(windows)
    build-depends: hakyll-sass

  default-language: Haskell2010
```

### 適用後

```cabal
cabal-version: 2.4
name:          test
version:       0.1.0.0
tested-with:
    GHC ==7.10.3
     || ==8.0.2
     || ==8.2.2
     || ==8.4.4
     || ==8.6.5
     || ==8.8.1
  , GHCJS ==8.4

executable site
  main-is:          site.hs
  other-modules:
    Config
    Hakyll.Ext

  hs-source-dirs:   app
  ghc-options:
    -Wcompat -Wall -Wnoncanonical-monad-instances
    -Wincomplete-uni-patterns -Wincomplete-record-updates
    -Wredundant-constraints -Wtabs -threaded

  build-depends:
    , base
    , blaze-html
    , bytestring
    , conduit-combinators
    , containers
    , extensible
    , filepath
    , lens
    , time
    , yaml

  if !os(windows)
    build-depends: hakyll-sass

  default-language: Haskell2010

```

## インストール

```shell
λ cabal v2-update
λ cabal v2-install cabal-fmt

λ cabal-fmt --help
λ cabal-fmt - .cabal file reformatter

Usage: cabal-fmt [-i|--inplace] [--Werror | --Wno-error | --indent N | --tabular
                 | --no-tabular] [FILE...] [--version]
  Reformat .cabal files

Available options:
  -i,--inplace             process files in-place
  --Werror                 Treat warnings as errors
  --indent N               Indentation
  --tabular                Tabular formatting
  FILE...                  input files
  -h,--help                Show this help text
  --version                Show version
```

## 使い方

基本的には以下のコマンドで終わりです。(`<proj>` は各自のファイル名に置き換えてください)

```shell
λ cabal-fmt <proj>.cabal -i
```

`-i` オプション (`--inplace`) で既存ファイルを上書きするので、結果だけ見たい場合は外しましょう。

### モジュールの自動展開

僕が `cabal-fmt` を使う理由の最大のポイントはモジュールの自動展開機能です。

```cabal
library
  -- cabal-fmt: expand src
  exposed-modules:
```

記法としては、上記のようにしてモジュールを探索するディレクトリ (今回の場合は **src**) を指定します。

実際に使ってみましょう。ここでは例として **test-dir** を作りますが、実際には **src**, **app**, **test** などが展開の対象となることが多いと思います。

また、意図的に **C.md** としている点にも注意してください。

```shell
λ mkdir test-dir
λ touch test-dir/A.hs test-dir/B.hs test-dir/C.md
λ tree test-dir
test-dir
├── A.hs
├── B.hs
└── C.md
```

適用する **cabal** ファイル (**test.cabal**) の中身は以下の通りです。

```cabal
name:    test
version: 0.1.0.0

library
  -- cabal-fmt: expand test-dir
  exposed-modules:
```

この状態で `cabal-fmt test.cabal -i` を実行すると結果は以下のようになります。

```cabal
name:    test
version: 0.1.0.0

library
  -- cabal-fmt: expand test-dir
  exposed-modules:
    A
    B
    C

```

`.hs` ファイルだけ列挙して欲しいところですが、指定したディレクトリ以下の全てを列挙します。そのため、公開したいモジュールと非公開のモジュールはディレクトリを分けて運用するなど、少しだけ工夫する必要があります。

一応、除外したいモジュールを `-Module` の形式で指定することもできます。

```cabal
name:    test
version: 0.1.0.0

library
  -- cabal-fmt: expand test-dir -C
  exposed-modules:
```

適用後

```cabal
name:    test
version: 0.1.0.0

library
  -- cabal-fmt: expand test-dir -C
  exposed-modules:
    A
    B

```

この方法を使えば **hspec-discover** や **tasty-discover** のためのファイルなどを除外することができます。

## まとめ

便利なので最近良く使ってます。

## 参考リソース

- [ANN: cabal-fmt](http://oleg.fi/gists/posts/2019-08-11-cabal-fmt.html)
