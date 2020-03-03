---
title: extra-deps の指定方法
published: 2019/09/14
# updated: 2019/09/14
prev: ./package-and-deps.html
next: ./create-app.html
---

**extra-deps** は実際の開発において必須の機能と言えます。ここで完全に使い方をマスターしましょう。

## extra-deps の利用方法

例えば今回の例は、以下のような状態です。

- スナップショットは **lts-14.5** を利用
- **lts-14.5** に含まれている **array** パッケージのバージョンは **0.5.3.0**

ここでは、例として1つ前のバージョン **array-0.5.2.0** を利用してみましょう。

ただし、**lts-14.5** には **array-0.5.3.0** しか含まれていないため、スナップショットに **array-0.5.2.0** を追加する必要があります。

そのためには **extra-deps** を利用します。

```yaml
resolver: lts-13.29
packages:
- .
extra-deps:
- array-0.5.2.0
```

これだけです。実際に確認してみましょう。

```shell
$ stack build
array> configure
array> Configuring array-0.5.2.0...
array> build
array> Preprocessing library for array-0.5.2.0..
array> Building library for array-0.5.2.0..
....

Completed 2 action(s).

$ stack ls dependencies
PFAD 0.1.0.0
array 0.5.2.0
base 4.12.0.0
ghc-prim 0.5.3
integer-gmp 1.0.2.0
rts 1.0
```

**array** のバージョンが **0.5.2.0** に変化したことを確認できました。

### extra-deps の注意点

**extra-deps** に追加した場合はビルドに失敗する可能性があります。今回はたまたま上手くいっただけです。

プロジェクトで利用しているパッケージの依存関係内において、バージョンの矛盾が起こらない限りビルド可能というのが **extra-deps** です。例えば過去の古い **GHC** に依存しているバージョンを指定した場合、他のパッケージが最新の **GHC** を要求していたら、ビルドできるはずありませんよね。

ただ、**extra-deps** を使って良くないことが起きるということは無いため、積極的に使ってみてください。ビルドできれば何の問題もありません。

実際のアプリケーション開発では必須の機能です。

## リモートリポジトリの指定

**extra-deps** が優秀なのは **Hackage** などに登録されていないけども **github** などにホスティングされている野良パッケージや、まだ **Hackage** に登録されていない開発バージョンのパッケージをとても簡単に利用できる点です。

具体例として [waddlaw/single-repo](https://github.com/waddlaw/single-repo) を使ってみましょう。

今回のために作ったサンプルパッケージなので、**Example** モジュールに **singleExample** 関数しか定義していません。

```haskell
module Example where

singleExample :: String
singleExample = "more single repo"
```

リポジトリのURLは **https://github.com/waddlaw/single-repo** になります。また、どのコミットハッシュを利用するか指定する必要があります。

ここでは一番最初のコミットハッシュ **9af5f4c6cdc03deb59bba03407321490f0a2aa41** を指定します。

```yaml
# stack.yaml
resolver: lts-14.5
packages:
- .
extra-deps:
- array-0.5.2.0
- git: https://github.com/waddlaw/single-repo       # この行を追加
  commit: 9af5f4c6cdc03deb59bba03407321490f0a2aa41  # この行を追加
```

あとは普通にビルドするだけです。

```shell
$ stack build
Cloning 9af5f4c6cdc03deb59bba03407321490f0a2aa41 from https://github.com/waddlaw/single-repo
PFAD> configure (lib + exe)
Configuring PFAD-0.1.0.0...
PFAD> build (lib + exe)
Preprocessing library for PFAD-0.1.0.0..
Building library for PFAD-0.1.0.0..
[2 of 2] Compiling Paths_PFAD
Preprocessing executable 'PFAD-exe' for PFAD-0.1.0.0..
Building executable 'PFAD-exe' for PFAD-0.1.0.0..
[2 of 2] Compiling Paths_PFAD
Linking .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/PFAD-exe/PFAD-exe ...
PFAD> copy/register
Installing library in /PFAD/.stack-work/install/x86_64-linux/e45e24d661a5523830acd90eb01354f05f6c2b4a1af4a66fed71976eed516247/8.6.5/lib/x86_64-linux-ghc-8.6.5/PFAD-0.1.0.0-JMjJGAZykCn4AzXbJSCMlb
Installing executable PFAD-exe in /PFAD/.stack-work/install/x86_64-linux/e45e24d661a5523830acd90eb01354f05f6c2b4a1af4a66fed71976eed516247/8.6.5/bin
Registering library for PFAD-0.1.0.0..
```

ログを見ると、ちゃんとリポジトリをクローンしているようですね。**package.yaml** に **single-repo** を追加して、依存関係をチェックしておきましょう。

```yaml
...
dependencies:
- base >= 4.7 && < 5
- array
- single-repo # ここを追加
...
```

```shell
$ stack ls dependencies
PFAD 0.1.0.0
array 0.5.2.0
base 4.12.0.0
ghc-prim 0.5.3
integer-gmp 1.0.2.0
rts 1.0
single-repo 0.1.0.0
```

実際に **stack ghci** で使ってみましょう。

```shell
$ stack ghci --no-load
λ import Example
λ singleExample
"single repo"
```

今度はコミットハッシュ値を変更して、もう一度実行してみます。そのために、**stack.yaml** の **extra-deps** を編集します。

**github** を参照する場合は **git** の代わりに **github** キーワード使えば、省略形が利用できるのでオススメです。コミットのハッシュ値も更新します。

```yaml
# stack.yaml
extra-deps:
- array-0.5.2.0
- github: waddlaw/single-repo                       # この行を変更
  commit: 16463931082172d6ad780034f6905e373dbc5139  # この行を変更
```

再び同じ手順で実行してみます。

```shell
$ stack ghci --no-load
...
λ import Example
λ singleExample
"more single repo"
```

ちゃんと異なるコミットのパッケージが利用できていますね。

## リモートリポジトリの設定 (mega-repo)

[amazonka](https://hackage.haskell.org/package/amazonka) など、いくつかのパッケージは単一のリポジトリで複数のパッケージを管理しています。そういったパッケージを **extra-deps** で使う場合は **subdirs** を指定する必要があります。

**mega-repo** 用のサンプルリポジトリを [waddlaw/mega-repo](https://github.com/waddlaw/mega-repo) に作りました。このリポジトリの内容は以下の通りです。

- mega-repo1 パッケージがある
- mega-repo2 パッケージがある

それぞれ、こんな感じの関数が定義されています。

```haskell
-- mega-repo1
module MegaExample1 where

megaExample1 :: String
megaExample1 = "mega repo example1"
```

```haskell
-- mega-repo2
module MegaExample2 where

megaExample2 :: String
megaExample2 = "mega repo example2"
```

では、実際に使ってみましょう。**extra-deps** に今回利用するパッケージを追記します。

```yaml
# stack.yaml
extra-deps:
- array-0.5.2.0
- github: waddlaw/single-repo
  commit: 16463931082172d6ad780034f6905e373dbc5139
- github: waddlaw/mega-repo                           # この行を追加
  commit: 130b4a853a6cdd1e2f2090b9a9a755ce30ee50d7    # この行を追加
```

試しにこのままビルドしてみましょう。

```shell
$ stack build
No cabal file found for Archive from https://github.com/waddlaw/mega-repo/archive/130b4a853a6cdd1e2f2090b9a9a755ce30ee50d7.tar.gz
```

こんな感じでビルドに失敗します。

ビルドを直すためには **subdirs** でパッケージを指定するだけです。

```yaml
# stack.yaml
extra-deps:
- github: waddlaw/single-repo
  commit: 16463931082172d6ad780034f6905e373dbc5139
- github: waddlaw/mega-repo
  commit: 130b4a853a6cdd1e2f2090b9a9a755ce30ee50d7
  subdirs:                                            # この行を追加
  - mega-repo1                                        # この行を追加
```

**ghci** で実際に動かしてみます。

```shell
$ stack ghci --package mega-repo1 --no-load
λ import MegaExample1
λ megaExample1
"mega repo example1"
```

現在 **subdirs** には **mega-repo1** しか指定していないため、**mega-repo2** パッケージは利用できません。

```shell
# subdirs で指定していないためエラーになった例
$ stack ghci --package mega-repo2 --no-load
Using main module: 1. Package `PFAD' component PFAD:exe:PFAD-exe with main-is file: /PFAD/app/Main.hs

Error: While constructing the build plan, the following exceptions were encountered:

Unknown package: mega-repo2

Some different approaches to resolving this:



Error: Plan construction failed.

Warning: Build failed, but trying to launch GHCi anyway
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: PFAD
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
<command line>: cannot satisfy -package mega-repo2
    (use -v for more information)
```

パッケージを複数指定する場合は、単純に **subdirs** を複数指定するだけです。

```yaml
extra-deps:
- github: waddlaw/single-repo
  commit: 16463931082172d6ad780034f6905e373dbc5139
- github: waddlaw/mega-repo
  commit: 130b4a853a6cdd1e2f2090b9a9a755ce30ee50d7
  subdirs:
  - mega-repo1
  - mega-repo2  # この行を追加
```

実行してみます。

```shell
$ stack ghci --package mega-repo1 --package mega-repo2
λ import MegaExample1
λ megaExample1
"mega repo example1"

λ import MegaExample2
λ megaExample2
"mega repo example2"
```

これでちゃんと実行できることが確認できました。
