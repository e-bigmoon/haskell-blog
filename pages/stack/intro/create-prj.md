---
title: プロジェクトの作成
date: 2019/09/14
prev: ./alt-stack.html
next: ./create-lib.html
---

**stack new** コマンドの後に**プロジェクト名**を指定することで、**Haskell** プロジェクトの雛形を作ることができます。

本チュートリアルでは **PFAD** というプロジェクト名を利用します。

```shell
$ stack new PFAD
...
All done.

$ cd PFAD
```

また、プロジェクトをビルドするためには **stack build** コマンドを実行します。

```shell
$ stack build
...
```

## 生成されたディレクトリとファイル

プロジェクトの作成直後は以下のようなディレクトリとファイルが生成されています。

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
```

これらのファイルの中身と役割を簡単に確認していきましょう。よく使う順に紹介しています。

### package.yaml

```yaml
name:                PFAD
version:             0.1.0.0
github:              "waddlaw/PFAD"
license:             BSD3
author:              "Shinya Yamguchi"
maintainer:          "example@example.com"
copyright:           "2019 Shinya Yamguchi"

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

本チュートリアルで頻繁に編集するファイルです。初期状態では **base** パッケージに含まれるモジュールのみが利用可能です。

### stack.yaml

```yaml
# コメントは削除しました
resolver: lts-14.5
packages:
- .
```

**stack.yaml** ファイルも非常に重要です。

主に以下の作業を行う場合に編集します。

- スナップショットの指定
- スナップショットに含まれないパッケージの指定

本チュートリアルで頻繁に編集するファイルです。

### PFAD.cabal

```
cabal-version: 1.12

-- This file has been generated from package.yaml by hpack version 0.31.2.
--
-- see: https://github.com/sol/hpack
--
-- hash: f04e2ae923fca8f3b4c22672f4a24ed1d088cfc9faafd914f923baeb9520ca73

name:           PFAD
version:        0.1.0.0
description:    Please see the README on GitHub at <https://github.com/waddlaw/PFAD#readme>
homepage:       https://github.com/waddlaw/PFAD#readme
bug-reports:    https://github.com/waddlaw/PFAD/issues
author:         Shinya Yamguchi
maintainer:     example@example.com
copyright:      2019 Shinya Yamguchi
license:        BSD3
license-file:   LICENSE
build-type:     Simple
extra-source-files:
    README.md
    ChangeLog.md

source-repository head
  type: git
  location: https://github.com/waddlaw/PFAD

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

このファイルに反映させたい内容については、**package.yaml** を編集することになります。また、ファイルの先頭に **hapck** のバージョンが追記されます。

```
-- This file has been generated from package.yaml by hpack version 0.31.2.
--
-- see: https://github.com/sol/hpack
--
-- hash: f04e2ae923fca8f3b4c22672f4a24ed1d088cfc9faafd914f923baeb9520ca73
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

本チュートリアルでは利用しませんが、実際のプロジェクトでは変更履歴をこのファイルに記録します。

### LICENSE

```
Copyright Shinya Yamguchi (c) 2019

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Shinya Yamguchi nor the names of other
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

新たに作成するソースコードは **app**, **src**, **test** に以下にそれぞれ配置します。一応、以下のコマンドに対応するようにフォルダを分けることが多いです。

ディレクトリ | 関連するコマンド | 備考
-------------|------------------|------------------------------
app | stack exec, stack run
bench | stack bench | デフォルトでは存在しない
haddock | stack haddock | デフォルトでは存在しない
src | stack build
test | stack test

**src/Lib.hs** ファイルは今回利用しないため削除しておきます。

```shell
$ rm src/Lib.hs
```

そうすると当然 **Lib.hs** が無いのでコンパイルエラーになってしまいます。

```shell
$ stack build
/PFAD/app/Main.hs:3:1: error:
    Could not load module `Lib'
    It is a member of the hidden package `libiserv-8.6.3'.
    Perhaps you need to add `libiserv' to the build-depends in your .cabal file.
    Use -v to see a list of the files searched for.
  |
3 | import Lib
  | 
```

とりあえず現状は **app/Main.hs** を以下のように書き換えておきましょう。

```hs
module Main (main) where

-- import Lib は削除

main :: IO ()
main = print "Hello World"
```

これでビルドが通るようになりました。

```shell
$ stack build
Building all executables for `PFAD' once. After a successful build of all of them, only specified executables will be rebuilt.
PFAD> build (lib + exe)
Preprocessing library for PFAD-0.1.0.0..
Building library for PFAD-0.1.0.0..
Preprocessing executable 'PFAD-exe' for PFAD-0.1.0.0..
Building executable 'PFAD-exe' for PFAD-0.1.0.0..
[1 of 2] Compiling Main
[2 of 2] Compiling Paths_PFAD
Linking .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/PFAD-exe/PFAD-exe ...
PFAD> copy/register
Installing library in /PFAD/.stack-work/install/x86_64-linux/754da54955212e5178bdb2a3208393df962e4e4e4998fe9886c862a9c58273a0/8.6.5/lib/x86_64-linux-ghc-8.6.5/PFAD-0.1.0.0-B1WTd1uDmyhGTpZWzSKOj5
Installing executable PFAD-exe in /PFAD/.stack-work/install/x86_64-linux/754da54955212e5178bdb2a3208393df962e4e4e4998fe9886c862a9c58273a0/8.6.5/bin
Registering library for PFAD-0.1.0.0..
```

## stack.yaml.lock ファイル

stack v2.1.3 から **stack.yaml.lock** ファイルというものが生成されるようになりました。

例えば、現在の **stack.yaml.lock** ファイルの中身は以下のようになっていると思います。

```yaml
# This file was autogenerated by Stack.
# You should not edit this file by hand.
# For more information, please see the documentation at:
#   https://docs.haskellstack.org/en/stable/lock_files

packages: []
snapshots:
- completed:
    size: 524104
    url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/14/5.yaml
    sha256: 3c146eebc1b383211ea49d5422fb3d63d699da12307b4bc989107300eb579d60
  original: lts-14.5
```

このファイルはビルドの**再現性**のため導入されました。

詳細については以下を参照してください。

- [Lock Files](https://docs.haskellstack.org/en/stable/lock_files/)
- [DISCUSS: Policy on including lock files in repo #4795](https://github.com/commercialhaskell/stack/issues/4795)

ここでの問題はこのファイルをリポジトリに含めるかどうかですが、今の所は **.gitignore** に追記しておいて良いです。

たぶん今後、パッケージの場合は **.gitignore** に含め、end product の場合はリポジトリに含めておくという方針になると思います。

このファイルは自動的に編集されるため、気にせず先に進みましょう。
