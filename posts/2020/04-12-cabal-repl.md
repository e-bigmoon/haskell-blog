---
title: cabal repl コマンドについて
author: Shinya Yamaguchi
tags: bigmoon, cabal
# updated: 2020/04/12
---

`cabal repl` コマンドは

- `ghci`, `ghc --interactive`
- `stack repl`, `stack ghci`

などと同じように **cabal** で **REPL** 環境を実行するためのコマンドです。

**cabal** プロジェクト内で実行する場合と、**cabal** プロジェクト外で実行する場合で挙動が少し異なります。

```shell
$ cabal -V
cabal-install version 3.2.0.0
compiled using version 3.2.0.0 of the Cabal library
```

<!--more-->

## **cabal** プロジェクト外で実行した場合

**cabal** は自動的に **fake-package** という環境を用意します。これは使い捨てのダミー **cabal** プロジェクトです。

```shell
$ cabal repl
Resolving dependencies...
Build profile: -w ghc-8.10.1 -O1
In order, the following will be built (use -v for more details):
 - fake-package-0 (lib) (first run)
Configuring library for fake-package-0..
Preprocessing library for fake-package-0..
Warning: No exposed modules
GHCi, version 8.10.1: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /tmp/cabal-repl.-3302/setcwd.ghci
Prelude>
```

生成される内容は [withoutProject](https://github.com/haskell/cabal/blob/cabal-install-v3.2.0.0/cabal-install/Distribution/Client/CmdRepl.hs#L392) の中で定義されています。また、ログメッセージの最後に表示されている `/tmp/cabal-repl.-3302/` ディレクトリに、実際に生成された **cabal** ファイルなどが保存されます。

```shell
$ ls /tmp/cabal-repl.-3302/
dist-newstyle  fake-package.cabal  setcwd.ghci
```

`cabal-install-3.2` で生成される `fake-package.cabal` の内容は以下のようになっています。

```
cabal-version: 2.2
name:          fake-package
version:       0

library
    default-language: Haskell2010
    build-depends:    base -any
```

**build-depends** に `base -any` が指定されているため、**GHC** のバージョンに対応した **base** パッケージがデフォルトで利用可能です。 

**REPL** の使い方は **ghci** などと同じなので大丈夫でしょう。

### **REPL** 起動時のメッセージを省略する

**REPL** 起動時のメッセージを省略するためには `-v0` オプションを指定します。

```shell
$ cabal repl -v0
Prelude>
```

### **REPL** で利用する GHC を切り替える

`cabal build` などでもおなじみの `-w` (`--with-compiler`) オプションを指定するだけです。

```shell
$ cabal repl -w ghc-8.8.3
...
GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /tmp/cabal-repl.-20727/setcwd.ghci
Prelude>

$ cabal repl -w ghc-8.10.1
...
GHCi, version 8.10.1: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /tmp/cabal-repl.-20914/setcwd.ghci
```

### ファイルを指定して **REPL** を起動

例えば `ghci A.hs` のように **REPL** 起動時にファイル (モジュール) を読み込みたい時があります。

しかし、これを **cabal** で同じように実行するとエラーになります。

```shell
$ cabal repl -v0 A.hs
cabal: 'repl' doesn't take any extra arguments when outside a project: A.hs
```

そのため、一度 `cabal repl` を実行してから `:l` コマンドでファイルを読み込む必要があります。

```shell
$ cabal repl -v0
Prelude> :l A.hs
*A>
```

### **REPL** にオプションを渡す

`stack repl` の `--ghci-options` に相当するオプションは `--repl-options` です。

```shell
$ cabal repl -v0 --repl-options="-XNoImplicitPrelude"
>
```

複数指定する場合は `--repl-options` を何度も指定する必要があります。たぶんこれは[仕様][cabal-issue-6190]っぽいです

```shell
$ cabal repl -v0 --repl-options="-XNoImplicitPrelude" --repl-options="-XNoStarIsType"
```

### **REPL** 依存関係を追加

**REPL** を動かす際、少し複雑なファイルの場合はいくつかの依存関係が追加で必要になることがあります。

そういう時には `-b` (`--build-depends`) オプションを利用します。このオプションは `stack repl --package` と同じような感じです。

**vector** パッケージを追加で読み込む例です。**vector** パッケージのビルドが走る場合があります。

```shell
$ cabal repl -b vector
...
Prelude> import Data.Vector
Prelude Data.Vector> 
```

複数のパッケージ **vector**, **aeson** を読み込む例は以下のようになります。

```shell
$ cabal repl -b vector -b aeson
...
Prelude> import Data.Vector
Prelude Data.Vector> import Data.Aeson
Prelude Data.Vector Data.Aeson>
```

このように、カンマ区切りで続けてパッケージを指定することも可能です。(`-b` ではエラーになります)

```shell
$ cabal repl -v0 --build-depends="vector, aeson"
Prelude>
```

パッケージのバージョンを指定することも可能です。バージョンの指定には **build-depends** と同じ記法が使えます。

```shell
$ cabal repl -b aeson==1.4.7.1
...
Prelude>
```

ここで、パッケージを追加した際は追加したパッケージが依存しているパッケージも含めて読み込まれる点に注意してください。(つまり、推移的に依存関係が追加されます)

例えば [deepseq](https://hackage.haskell.org/package/deepseq) パッケージで定義されている `Control.DeepSeq` は本当なら **import** できないはずです。しかし、[vector](https://hackage.haskell.org/package/vector) パッケージは **deepseq** に依存しているため **import** できてしまいます。

```shell
$ cabal repl -v0 -b vector
Prelude> import Control.DeepSeq 
Prelude Control.DeepSeq>
```

この挙動を変更したい場合は `--no-transitive-deps` オプションを指定します。

```shell
$ cabal repl -v0 -b vector --no-transitive-deps
Prelude> import Control.DeepSeq 

<no location info>: error:
    Could not load module ‘Control.DeepSeq’
    It is a member of the hidden package ‘deepseq-1.4.4.0’.
    Perhaps you need to add ‘deepseq’ to the build-depends in your .cabal file.
```

読み込まれるパッケージがどのように変化しているか確認しておきましょう。

```shell
$ cabal repl -v0
Prelude> :show packages 
active package flags:
  -package-id base-4.14.0.0

$ cabal repl -v0 -b vector
Prelude> :show packages 
active package flags:
  -package-id transformers-0.5.6.2
  -package-id primitive-0.7.0.1-26f169240ac34903846f3a33e2517e97382af71bd7fe7cb7262fd6ca4381f53e
  -package-id array-0.5.4.0
  -package-id deepseq-1.4.4.0
  -package-id integer-gmp-1.0.3.0
  -package-id rts
  -package-id ghc-prim-0.6.1
  -package-id vector-0.12.1.2-8dc77cdd80241a59f0811ecc4c067ae98900f833977f73cce9795875f4909da9
  -package-id base-4.14.0.0

$ cabal repl -v0 -b vector --no-transitive-deps
Prelude> :show packages 
active package flags:
  -package-id vector-0.12.1.2-8dc77cdd80241a59f0811ecc4c067ae98900f833977f73cce9795875f4909da9
  -package-id base-4.14.0.0
```

## **cabal** プロジェクト内で実行した場合

基本的にはプロジェクト内で `cabal repl` と実行するとプロジェクトの **cabal** ファイルに従い

- 依存関係の追加
- モジュールの追加
- オプションの追加

などを自動的に行い、**REPL** を起動します。

```shell
$ cabal repl -v0
*MyLib> 
```

この時、**import** されるモジュールは1つのみです。(これは仕様みたいです)

そのため、必要なモジュールは自分で **import** する必要があります。

### プロジェクトを無視して **REPL** を起動

プロジェクトを無視して **REPL** を起動するためには `-z` (`--ignore-project`) オプションを指定します。

```shell
$ cabal repl -v0 -z
Prelude>
```

## 今回紹介したオプション一覧

short | long | オプションの意味 | stack
------|-------|---------------|--------
 無し | `--repl-options` | **ghci** に渡すオプション指定 | `--ghci-options`
`-b` | `--build-depends` | 依存関係の追加 | `--package`
`-z` | `--ignore-project` | プロジェクトの **cabal** を無視する | `--no-load` が近いけど<br>プロジェクトの依存関係は読み込む
 無し | `--no-transitive-deps` | 推移的な依存関係を追加しない | デフォルトの動作
`-w` | `--with-compiler` | 利用するコンパイラの指定 | `-with-ghc`?
`-v0` | `--verbose=0` | 起動時のメッセージを省略 | 無し

## 参考リソース

- [5.4.4. cabal v2-repl - Cabal User Manual](https://www.haskell.org/cabal/users-guide/nix-local-build.html#cabal-v2-repl)
- [Add --build-depends flag, associated support to new-repl #5454][cabal-issue-5454]
- [Cabal repl and cabal build/run conflict over .o object files if ghci has -fobject-code set #3565][cabal-issue-3565]
- [cabal repl load single module #2592][cabal-issue-2592]
- [--repl-options doesn’t split on whitespace #6190][cabal-issue-6190]
- [Allow list for `repl --build-depends` #5845][cabal-issue-5845]
- [`cabal new-repl` only brings one module into scope #5374][cabal-issue-5347]

[cabal-issue-5454]: https://github.com/haskell/cabal/pull/5454
[cabal-issue-3565]: https://github.com/haskell/cabal/pull/3565
[cabal-issue-2592]: https://github.com/haskell/cabal/pull/2592
[cabal-issue-6190]: https://github.com/haskell/cabal/pull/6190
[cabal-issue-5845]: https://github.com/haskell/cabal/pull/5845
[cabal-issue-5347]: https://github.com/haskell/cabal/issues/5374