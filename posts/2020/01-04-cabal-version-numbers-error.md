---
title: Memo - unexpected At most 9 numbers are allowed per version number part
author: Shinya Yamaguchi
tags: bigmoon, cabal
updated: 2020/01/22
---

GHC-8.8 への移行中に GitHub Actions で以下のようなエラーが発生しました。

```shell
proj/dist-newstyle/src/barbies-6f24224e2c384e2f/barbies.cabal:2:31: error:
unexpected At most 9 numbers are allowed per version number part

    1 | name:           barbies
    2 | version:        1.999999999999
      |                               ^

##[error]Process completed with exit code 1.
```

初めて見るエラーだったのでメモとして残しておきます。

<!--more-->

## エラーが発生した理由

エラー発生の原因はたぶんこれです。

- [Limit version number parts to be 9 digits #6386](https://github.com/haskell/cabal/pull/6386)

この変更は `cabal-3.0.1.0` に含まれるはずなので `cabal-3.0.0.0` では関係ないと思っていたのですが、[GitHub Actions のログ](https://github.com/e-bigmoon/haskell-blog/commit/73a7adcb8bf8204dd81de35d014c967b76040526/checks?check_suite_id=384131716)を見るとわかるとおり、GitHub Actions では `cabal-3.0.1.0` がインストールされていました。(何故?[cabal-install 3.0.1.0 release planning #6328](https://github.com/haskell/cabal/issues/6328) はまだ閉じられてない)

```shell
...
The Glorious Glasgow Haskell Compilation System, version 8.6.5
cabal-install version 3.0.1.0
compiled using version 3.0.1.0 of the Cabal library 
...
```

また [actions/setup-haskell](https://github.com/actions/setup-haskell) の README を見る限り `cabal` のバージョン指定方法は `2.0`, `2.2`, `2.4`, `3.0` しか無いので `3.0.0.0` を使う方法がありません・・・。

## とりあえずの解決策

[barbies.cabal](https://github.com/jcpetruzza/barbies/blob/0b09a4235cd719aa3df9f9467f3e8fee81446bc8/barbies.cabal) のバージョン番号が長すぎるために起きているエラーなので、fork して一時的にバージョンを短くして対応しました。

```
name:           barbies
version:        1.999999999999  -- 変更前
version:        1.999999999     -- 変更後
```

## まとめ

もうすぐ GHC-8.10.1 がリリースされそうですね。

- [GHC plans for 8.10.1](https://gitlab.haskell.org/ghc/ghc/wikis/status/ghc-8.10.1)