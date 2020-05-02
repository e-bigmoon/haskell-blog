---
title: stack で管理されたプロジェクトを cabal でビルドするために
author: Shinya Yamaguchi
tags: bigmoon, cabal
updated: 2020/05/02
---

Haskell のビルドツールといえば **cabal** と **stack** です。ちょっと前までは **cabal** より **stack** の方が流行っていたのですが、最近は開発も落ち着いているようであまり動きがありません。それよりも **cabal** の **nix-style local build** が非常に使いやすく、近頃では **stack** から **cabal** に移行しているプロジェクトも多くあります。

**stack** を使っていて改善したら良いなぁと思う部分としては、新しい **GHC** やライブラリをすぐに使おうと思っても **LTS** や **nightly** に入っていないため使えなかったり、**Backpack** が未だに使えなかったりするところでしょうか。

ただ、ビルドの再現性という点においては **stack** の方が優秀だと思っているので今は両方使っています。(**index-state** と **freeze** ファイルを組み合わせれば **cabal** でも再現性が保証されそうな気がしますが、どうなんだろう)

今回は **stack** で管理されたプロジェクトを確実に **cabal** でビルドするための方法についてまとめました。

- [Why Not Both?](https://medium.com/@fommil/why-not-both-8adadb71a5ed) に載ってた **Stackage** の使い方の紹介でもあります。

<!--more-->

## モチベーション

ここでは **stack** で管理されていて、**cabal** ファイルにバージョン制約が明記されていないという、良くあるシチュエーションを考えます。

どんなプロジェクトでも良いのですが、具体的には [arcticmatt/dino-brick](https://github.com/arcticmatt/dino-brick) のようなプロジェクトです。`stack.yaml` は以下のようになっています。

```yaml
resolver: lts-8.23
packages:
- '.'
```

[dino.cabal の build-depends](https://github.com/arcticmatt/dino-brick/blob/dino/dino.cabal#L16) には、ほとんどバージョンが明記されていません。(**stack** の場合はスナップショットが決まると自動的にパッケージのバージョンが決まるため、明示する必要はあまり無いのです)

```yaml
library:
  build-depends:
      base >= 4.7 && < 5
    , brick
    , containers
    , linear
    , microlens
    , microlens-th
    , random
    , vty
    , MonadRandom
```

このプロジェクトを **cabal** でビルドするためにはどうしたら良いんだろうか？というお話です。何もしなくてもビルドできるプロジェクトも結構あるんですが、ハマる時もあります・・・。

## package.yaml から cabal ファイルを生成する

リポジトリに `package.yaml` しか含まれていない場合は、`package.yaml` から **cabal** ファイルを生成しましょう。

以下のコマンドでビルドすることなくすぐに生成できます。

```shell
$ stack build --dry-run
```

今回は `dino.cabal` が最初からリポジトリに含まれているため何もしなくても良さそうですね。

## cabal でビルドしてみよう

とりあえずビルドしてみましょう。

```shell
$ git clone https://github.com/arcticmatt/dino-brick.git
$ cd dino-brick

$ cabal update
Downloading the latest package list from hackage.haskell.org
To revert to previous state run:
    cabal v2-update 'hackage.haskell.org,2020-01-19T06:12:36Z'

$ cabal build
...

src/UI.hs:142:56: error:
    • Couldn't match expected type ‘App s0 e0 n0’
                  with actual type ‘Game’
    • In the fourth argument of ‘customMain’, namely ‘g’
      In a stmt of a 'do' block:
        customMain (V.mkVty V.defaultConfig) (Just chan) app g
      In the expression:
        do chan <- newBChan 10
           forkIO
             $ forever
                 $ do modifyIORef counter (+ 1)
                      c' <- readIORef counter
                      ....
           g <- initGame 0
           customMain (V.mkVty V.defaultConfig) (Just chan) app g
    |
142 |   customMain (V.mkVty V.defaultConfig) (Just chan) app g
    |                                                        ^
cabal: Failed to build dino-0.1.0.0 (which is required by exe:dino from
dino-0.1.0.0).
```

エラーがいくつも出てしまいました。上記の結果はそのうちの最後の1つだけを表示しています。

ちなみに `stack build` だとビルドできます。

### 問題点

**stack** だとビルドできて、**cabal** だと失敗してしまう原因はビルド時にパッケージのバージョンにあります。どのパッケージが原因かと言うと、今回は [brick](https://hackage.haskell.org/package/brick) です。

**stack** の場合は [LTS-8.23](https://www.stackage.org/lts-8.23) に含まれるバージョンを利用することになるので [brick-0.17.2](https://www.stackage.org/lts-8.23/package/brick-0.17.2) を利用します。

一方で **cabal** の場合は `cabal update` を最後に実行した時の **Hackage** の最新バージョンが利用されます。これは明示的なバージョン制約が **cabal** ファイルに記述されていないためです。そのため [brick-0.50.1](https://hackage.haskell.org/package/brick-0.50.1) などが利用されます。

バージョンアップによって後方互換性が保たれている場合は何も考えずにビルドが通るのですが、**GHC** のバージョンが変わるタイミングなどでは破壊的変更が含まれている場合も多いため、どこかでビルドが壊れます。

今回のプロジェクトでは [brick-0.47](https://github.com/jtdaugherty/brick/blob/master/CHANGELOG.md#047) の変更によって `Brick.Main.customMain` の型が変わり、その結果ビルドエラーになりました。

```haskell
-- 0.46
customMain :: Ord n =>        IO Vty -> Maybe (BChan e) -> App s e n -> s -> IO s

-- 0.47
customMain :: Ord n => Vty -> IO Vty -> Maybe (BChan e) -> App s e n -> s -> IO s
```

このように、原因が特定できれば、修正は簡単です。`brick` にバージョン制約を付けるだけです。(0.47 でビルドできるようにコードを修正する方法ももちろん考えられます)

```cabal
library:
  build-depends:
      base >= 4.7 && < 5
    , brick == 0.46       -- 破壊的変更が起きる前のバージョンを指定
    , containers
    , linear
    , microlens
    , microlens-th
    , random
    , vty
    , MonadRandom
```

このプロジェクトはこれで上手く動きました。

しかし、どのバージョンで壊れたかどうかを毎回調べるのはかなりつらいです。そのため、もっと良い方法として `LTS-8.23` のバージョン制約を使ってみましょう。

## cabal freeze コマンド

**cabal** には `cabal freeze` というコマンドがあります。アプリケーション開発で便利な機能です。

コマンドを実行すると `cabal.project.freeze` というファイルが作られます。

```shell
$ cabal freeze
Wrote freeze file: dino-brick/cabal.project.freeze
```

このファイルは一言でいえば `npm` の `package-lock.json` ファイルと同じです。ビルドの再現性を保証するためのものです。

例えば、先ほどの `dino.cabal` ファイルで `brick` のバージョンを `^>= 0.46` のように指定した場合を考えてみましょう。この指定方法は `brick >= 0.46 && < 0.47` と同じ意味になります。(**cabal 2.0** から利用可能な記法です)

```cabal
library:
  build-depends:
      base >= 4.7 && < 5
    , brick ^>= 0.46      -- brick >= 0.46 && < 0.47 と同じ意味
    , containers
    , linear
    , microlens
    , microlens-th
    , random
    , vty
    , MonadRandom
```

例えば、会社のデスクトップPCでビルドしたときに `brick-0.46` がインストールされたとしましょう。

次の日の朝、バグフィックスされた `brick-0.46.1` が **Hackage** にアップロードされました。

その日の午後、自宅のノートPCで `cabal update && cabal build` を行った場合、インストールされるのは `brick-0.46.1` になります。

つまり、`brick ^>= 0.46` という指定方法では環境ごとに同じバージョンが使われていることを保証できません。そのため、`cabal freeze` コマンドで `cabal.project.freeze` を生成し、コマンドを実行した環境で実際に利用されている具体的なバージョンを記録しておきます。これは **stack** のスナップショットと同じようなものです。

実際に生成されたファイルの中身はこんな感じです。

```cabal
constraints: any.Cabal ==2.4.0.1,
             any.MonadRandom ==0.5.1.2,
             any.QuickCheck ==2.13.2,
             QuickCheck +templatehaskell,
             any.StateVar ==1.2,
             any.adjunctions ==4.4,
             any.ansi-terminal ==0.10.2,
             ansi-terminal -example,
             any.ansi-wl-pprint ==0.6.9,
             ansi-wl-pprint -example,
             any.array ==0.5.3.0,
             any.base ==4.12.0.0,
             any.base-orphans ==0.8.1,
             any.bifunctors ==5.5.6,
             bifunctors +semigroups +tagged,
             any.binary ==0.8.6.0,
             any.binary-orphans ==1.0.1,
             any.blaze-builder ==0.4.1.0,
             any.brick ==0.46,
             brick -demos,
             any.bytes ==0.16,
             bytes +test-doctests,
             any.bytestring ==0.10.8.2,
             any.cabal-doctest ==1.0.8,
             any.call-stack ==0.2.0,
             any.case-insensitive ==1.2.1.0,
             any.cereal ==0.5.8.1,
             cereal -bytestring-builder,
             any.colour ==2.3.5,
             any.comonad ==5.0.6,
             comonad +containers +distributive +test-doctests,
             any.config-ini ==0.2.4.0,
             config-ini -enable-doctests,
             any.containers ==0.6.0.1,
             any.contravariant ==1.5.2,
             contravariant +semigroups +statevar +tagged,
             any.data-clist ==0.1.2.3,
             any.deepseq ==1.4.4.0,
             any.directory ==1.3.3.0,
             any.distributive ==0.6.1,
             distributive +semigroups +tagged,
             any.dlist ==0.8.0.7,
             any.exceptions ==0.10.4,
             exceptions +transformers-0-4,
             any.filepath ==1.4.2.1,
             any.free ==5.1.3,
             any.ghc-boot-th ==8.6.5,
             any.ghc-prim ==0.5.3,
             any.hashable ==1.3.0.0,
             hashable -examples +integer-gmp +sse2 -sse41,
             any.integer-gmp ==1.0.2.0,
             any.integer-logarithms ==1.0.3,
             integer-logarithms -check-bounds +integer-gmp,
             any.invariant ==0.5.3,
             any.kan-extensions ==5.2,
             any.lens ==4.18.1,
             lens -benchmark-uniplate -dump-splices +inlining -j -old-inline-pragmas -safe +test-doctests +test-hunit +test-properties +test-templates +trustworthy,
             any.linear ==1.20.9,
             linear -herbie +template-haskell,
             any.megaparsec ==7.0.5,
             megaparsec -dev,
             any.microlens ==0.4.11.2,
             any.microlens-mtl ==0.2.0.1,
             any.microlens-th ==0.4.3.2,
             any.mtl ==2.2.2,
             any.optparse-applicative ==0.15.1.0,
             any.parallel ==3.2.2.0,
             any.parsec ==3.1.13.0,
             any.parser-combinators ==1.2.1,
             parser-combinators -dev,
             any.pretty ==1.1.3.6,
             any.primitive ==0.7.0.0,
             any.process ==1.6.5.0,
             any.profunctors ==5.5.1,
             any.random ==1.1,
             any.reflection ==2.1.5,
             reflection -slow +template-haskell,
             any.rts ==1.0,
             any.scientific ==0.3.6.2,
             scientific -bytestring-builder -integer-simple,
             any.semigroupoids ==5.3.4,
             semigroupoids +comonad +containers +contravariant +distributive +doctests +tagged +unordered-containers,
             any.semigroups ==0.19.1,
             semigroups +binary +bytestring -bytestring-builder +containers +deepseq +hashable +tagged +template-haskell +text +transformers +unordered-containers,
             any.splitmix ==0.0.3,
             splitmix -optimised-mixer +random,
             any.stm ==2.5.0.0,
             any.tagged ==0.8.6,
             tagged +deepseq +transformers,
             any.template-haskell ==2.14.0.0,
             any.terminfo ==0.4.1.2,
             any.text ==1.2.3.1,
             any.text-zipper ==0.10.1,
             any.th-abstraction ==0.3.1.0,
             any.time ==1.8.0.2,
             any.transformers ==0.5.6.2,
             any.transformers-base ==0.4.5.2,
             transformers-base +orphaninstances,
             any.transformers-compat ==0.6.5,
             transformers-compat -five +five-three -four +generic-deriving +mtl -three -two,
             any.type-equality ==1,
             any.unix ==2.7.2.2,
             any.unordered-containers ==0.2.10.0,
             unordered-containers -debug,
             any.utf8-string ==1.0.1.1,
             any.vector ==0.12.0.3,
             vector +boundschecks -internalchecks -unsafechecks -wall,
             any.void ==0.7.3,
             void -safe,
             any.vty ==5.26,
             any.word-wrap ==0.4.1
```

`cabal.project.freeze` ファイルと `<project>.cabal` ファイルでバージョンが異なる場合は `<project>.cabal` のバージョンが優先されるようです。

```cabal
library:
  build-depends:
      base >= 4.7 && < 5
    , brick ^>= 0.47      -- ビルドが失敗するバージョン制約を指定
    , containers
    , linear
    , microlens
    , microlens-th
    , random
    , vty
    , MonadRandom
```

```shell
$ cabal build
...
[__1] fail (backjumping, conflict set: brick, dino)
After searching the rest of the dependency tree exhaustively, these were the
goals I've had most trouble fulfilling: brick, dino

$ cabal freeze
...
[__1] fail (backjumping, conflict set: brick, dino)
After searching the rest of the dependency tree exhaustively, these were the
goals I've had most trouble fulfilling: brick, dino
```

そもそも制約を満たさない場合は `cabal freeze` が失敗するみたいです。

## スナップショットに対応した freeze ファイルを使おう

さて、それではリポジトリを **clone** した直後に戻しましょう。こんな状態です。

```shell
$ git clone https://github.com/arcticmatt/dino-brick.git
$ cd dino-brick
$ cabal update
```

```yaml
library:
  build-depends:
      base >= 4.7 && < 5
    , brick
    , containers
    , linear
    , microlens
    , microlens-th
    , random
    , vty
    , MonadRandom
```

**Stackage** のスナップショットの **URL** の後ろに `cabal.config` を付けた [https://www.stackage.org/lts-8.23/cabal.config](https://www.stackage.org/lts-8.23/cabal.config) にアクセスすると `cabal.project.freeze` ファイルとして利用可能なテキストファイルが表示されます。

これをそのまま保存してビルドするだけで全てが上手くいきます。

```shell
$ curl https://www.stackage.org/lts-8.23/cabal.config > cabal.project.freeze
$ cabal build
[__2] fail (backjumping, conflict set: base, dino, optparse-applicative)
After searching the rest of the dependency tree exhaustively, these were the
goals I've had most trouble fulfilling: optparse-applicative, base, dino
```

おっと忘れていました。`LTS-8.23` は `GHC-8.0.2` でしたね。

`-w` (`with-compiler` の頭文字) オプションで利用する **GHC** を切り替えてビルドしましょう！

```shell
$ cabal build -w ghc-8.0.2
```

## まとめ

- **stack** でビルドが通っていれば、**cabal** でも通る
- `cabal freeze` を使うとスナップショットのようにバージョンを記録できる
- **Stackage** のスナップショットの **URL** の最後に `cabal.config` を付けると **freeze** ファイルを取得できる

## 参考リソース

- [5.4.6. cabal v2-freeze](https://www.haskell.org/cabal/users-guide/nix-local-build.html#cabal-v2-freeze)
- [Why Not Both?](https://medium.com/@fommil/why-not-both-8adadb71a5ed)