---
title: stack run コマンド
author: Shinya Yamaguchi
tags: bigmoon, extensible
---

## はじめに

先日 stack の master ブランチに [Introduce stack run command line option #3952](https://github.com/commercialhaskell/stack/pull/3952) がマージされました。とても便利なので紹介したいと思います。

**stack run** コマンドについて簡単に説明するとこんな感じです。

- **cabal run** の **stack** バージョンです。
- `stack build && stack exec foo` の短縮形です。

```sh
$ stack --version
Version 1.8.0, Git revision 466da22ce21cddc20323fc2611cd2a2d3cc56ec6 (dirty) (5998 commits) x86_64 hpack-0.28.2
```

現在の **stack** の最新バージョンは **1.7.1** です。

<!--more-->

## stack run コマンド

実際にプロジェクトを作って試してみましょう。

```shell
$ stack new stack-run-test
$ cd stack-run-test
```

初回は通常通りビルドが実行され、その後に someFunc が出力されます。

```shell
$ stack run
Building all executables for `stack-run-test' once. After a successful build of all of them, only specified executables

...

Registering library for stack-run-test-0.1.0.0..
someFunc
```

2回目以降はビルド結果がキャッシュされているので、ファイルを変更しなれければ再ビルドされることはありません。

```shell
$ stack run
someFunc
```

## まとめ

`stack run` コマンドは [Wishlist: stack run #233](https://github.com/commercialhaskell/stack/issues/233) によって2015年6月に提案されているので3年越しの実装ということになります。この調子で色々と便利なコマンドが増えると良いですね。

実行ファイル名の指定を省略できるのは、非常に便利なので僕も使っていこうと思います。

以上です。