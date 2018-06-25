---
title: プロジェクトの作成
date: 2018/05/05
prev: ./hpack.html
next: ./create-lib.html
---

## プロジェクトの作り方

**stack new** コマンドの後に**プロジェクト名**を指定することで、**Haskell** プロジェクトの雛形を作ることができます。

本チュートリアルでは **PFAD** というプロジェクト名を利用します。

```shell
$ stack new PFAD
$ cd PFAD
```

### ビルド

プロジェクトをビルドするためには **stack build** コマンドを実行します。

```shell
$ stack build
...
```

## 生成されたディレクトリとファイル

プロジェクトを作った直後は以下のようなディレクトリとファイルが生成されています。

```shell
$ tree .
.
├── ChangeLog.md
├── LICENSE
├── PFAD.cabal
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── package.yaml
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs

3 directories, 10 files
```

これらのファイルの中身と役割を簡単に確認していきましょう。よく使う順に紹介しています。

### package.yaml

```yaml
name:                PFAD
version:             0.1.0.0
github:              "e-bigmoon/PFAD"
license:             BSD3
author:              "BIG MOON"
maintainer:          "example@example.com"
copyright:           "2018 BIG MOON"

extra-source-files:
- README.md
- ChangeLog.md

# Metadata used when publishing your package
# synopsis:            Short description of your package
# category:            Web

# To avoid duplicated efforts in documentation and dealing with the
# complications of embedding Haddock markup inside cabal files, it is
# common to point users to the README.md file.
description:         Please see the README on GitHub at <https://github.com/waddlaw/PFAD#readme>

dependencies:
- base >= 4.7 && < 5

library:
  source-dirs: src

executables:
  PFAD-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD

tests:
  PFAD-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
```

このファイルはとても大事です。`.cabal` ファイルに設定する内容は全てこのファイルに記述します。

主に以下の作業を行う場合に編集します。

- 新しい依存関係を追加
- 全てのファイルで有効となる言語拡張の追加
- **ghc-options** の追加
- ビルドフラグの追加

本チュートリアルで頻繁に編集するファイルです。

初期状態では **base** パッケージのみが利用可能です。

### stack.yaml

```yaml
resolver: lts-11.7
packages:
- .
```

上記は、コメントは削除した結果です。

このファイルも非常に重要です。

主に以下の作業を行う場合に編集します。

- スナップショットの指定
- スナップショットに含まれないパッケージの指定

本チュートリアルで頻繁に編集するファイルです。

### PFAD.cabal

```
-- This file has been generated from package.yaml by hpack version 0.28.2.
--
-- see: https://github.com/sol/hpack
--
-- hash: 9660bd5c46ca581a672c6c267336595fe7bd275fd4e774bd9b74cbfd70dce5e3

name:           PFAD
version:        0.1.0.0
description:    Please see the README on GitHub at <https://github.com/waddlaw/PFAD#readme>
homepage:       https://github.com/waddlaw/PFAD#readme
bug-reports:    https://github.com/waddlaw/PFAD/issues
author:         BIG MOON
maintainer:     example@example.com
copyright:      2018 BIG MOON
license:        BSD3
license-file:   LICENSE
build-type:     Simple
cabal-version:  >= 1.10
extra-source-files:
    ChangeLog.md
    README.md

source-repository head
  type: git
  location: https://github.com/e-bigmoon/PFAD

library
  exposed-modules:
      Lib
  other-modules:
      Paths_PFAD
  hs-source-dirs:
      src
  build-depends:
      base >=4.7 && <5
  default-language: Haskell2010

executable PFAD-exe
  main-is: Main.hs
  other-modules:
      Paths_PFAD
  hs-source-dirs:
      app
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      PFAD
    , base >=4.7 && <5
  default-language: Haskell2010

test-suite PFAD-test
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  other-modules:
      Paths_PFAD
  hs-source-dirs:
      test
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      PFAD
    , base >=4.7 && <5
  default-language: Haskell2010
```

**hpack** を利用するため、本チュートリアルでは直接利用することはありません。

このファイルに反映させたい内容については、後述の **package.yaml** を編集することになります。

また、ファイルの先頭に **hapck** のバージョンが追記されます。

```
-- This file has been generated from package.yaml by hpack version 0.28.2.
--
-- see: https://github.com/sol/hpack
--
-- hash: 9660bd5c46ca581a672c6c267336595fe7bd275fd4e774bd9b74cbfd70dce5e3
```

### README.md

```md
# PFAD
```

よくある **README.md** ファイルです。

実際のプロジェクトではビルド方法や各種リンクが張られていたりします。

### ChangeLog.md

```md
# Changelog for PFAD

## Unreleased changes
```

プロジェクトの変更履歴を管理するためのファイルです。

本チュートリアルでは利用しませんが、実際のプロジェクトでは変更履歴をこのファイルに記録しておくことは非常に有用です。


### LICENSE

```
Copyright BIGMOON (c) 2018

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of BIGMOON nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

必要に応じて、適したライセンスを準備しましょう。本チュートリアルでは利用しません。

### Setup.hs

```hs
import Distribution.Simple
main = defaultMain
```

本チュートリアルでは利用しません。実際の開発でもこのファイルを編集することは稀だと思います。

## 準備

**stack** で生成したプロジェクトの雛形を使う場合は慣習として、新たに作成するソースコードは **app**, **src**, **test** 以下にそれぞれ配置します。

各ディレクトリは以下の表のように **stack** のサブコマンドと関連しています。

ディレクトリ | 関連するコマンド | 保存するソースコードの種類
-------------|------------------|------------------------------
app | stack exec | アプリケーション (`main` 関数を含むファイル)
src | stack build | ライブラリ
test | stack test | テスト

**src/Lib.hs** ファイルは今回利用しないため削除しておきます。

```shell
$ rm src/Lib.hs
```

そうすると、このままビルドしようとすると **Lib.hs** が見つからないという理由でコンパイルエラーになってしまいます。

```shell
$ stack build
Building all executables for `PFAD' once. After a successful build of all of them, only specified executables will be rebuilt.
PFAD-0.1.0.0: configure (lib + exe)
Configuring PFAD-0.1.0.0...
PFAD-0.1.0.0: build (lib + exe)
Preprocessing library for PFAD-0.1.0.0..
Building library for PFAD-0.1.0.0..
[1 of 1] Compiling Paths_PFAD       ( .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/autogen/Paths_PFAD.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Paths_PFAD.o )
Preprocessing executable 'PFAD-exe' for PFAD-0.1.0.0..
Building executable 'PFAD-exe' for PFAD-0.1.0.0..
[1 of 2] Compiling Main             ( app/Main.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/PFAD-exe/PFAD-exe-tmp/Main.o )

/Users/bm12/Desktop/PFAD/app/Main.hs:3:1: error:
    Could not find module ‘Lib’
    Use -v to see a list of the files searched for.
  |
3 | import Lib
  | ^^^^^^^^^^


--  While building custom Setup.hs for package PFAD-0.1.0.0 using:
      /Users/bm12/.stack/setup-exe-cache/x86_64-osx/Cabal-simple_mPHDZzAJ_2.0.1.0_ghc-8.2.2 --builddir=.stack-work/dist/x86_64-osx/Cabal-2.0.1.0 build lib:PFAD exe:PFAD-exe --ghc-options " -ddump-hi -ddump-to-file -fdiagnostics-color=always"
    Process exited with code: ExitFailure 1
```

とりあえず現状は **src/Main.hs** を以下のように書き換えておきましょう。

```hs
module Main (main) where

main :: IO ()
main = undefined
```

そうすればコンパイルが無事に通るようになります。

```shell
$ stack build
Building all executables for `PFAD' once. After a successful build of all of them, only specified executables will be rebuilt.
PFAD-0.1.0.0: build (lib + exe)
Preprocessing library for PFAD-0.1.0.0..
Building library for PFAD-0.1.0.0..
Preprocessing executable 'PFAD-exe' for PFAD-0.1.0.0..
Building executable 'PFAD-exe' for PFAD-0.1.0.0..
[1 of 2] Compiling Main             ( app/Main.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/PFAD-exe/PFAD-exe-tmp/Main.o )
[2 of 2] Compiling Paths_PFAD       ( .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/PFAD-exe/autogen/Paths_PFAD.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/PFAD-exe/PFAD-exe-tmp/Paths_PFAD.o )
Linking .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/PFAD-exe/PFAD-exe ...
PFAD-0.1.0.0: copy/register
Installing library in /Users/bm12/Desktop/PFAD/.stack-work/install/x86_64-osx/lts-11.7/8.2.2/lib/x86_64-osx-ghc-8.2.2/PFAD-0.1.0.0-J6iy8SXRSjD3dsGGyhgUYc
Installing executable PFAD-exe in /Users/bm12/Desktop/PFAD/.stack-work/install/x86_64-osx/lts-11.7/8.2.2/bin
Registering library for PFAD-0.1.0.0..
```