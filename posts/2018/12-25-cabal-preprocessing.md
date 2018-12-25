---
title: cabal build で package.yaml を使う
author: Shinya Yamaguchi
tags: bigmoon,cabal
---

## はじめに

最近すこしずつ `cabal` を使うようになりました。

そのため、`stack` と `cabal` のどちらを使ってもビルドできるようにプロジェクトを修正していたのですが、`cabal` ファイルの取り扱いが難しかったのでメモ程度に残しておきます。

方針としては `cabal` コマンドの実行時に `Hook` を仕掛けて `package.yaml` から `cabal` ファイルを生成しようという感じです。

```hs
$ cabal --numeric-version
2.4.1.0
```

<!--more-->

## Setup.hs は何のためにあるのか？

`stack` を使ってプロジェクトを作ると以下の内容で `Setup.hs` が自動的に作られると思います。

```hs
import Distribution.Simple
main = defaultMain
```

今まで特に使わなくても困らなかったので、気にせずに放置していましたが、このファイルが今回の主役になります。

`cabal` には [build-type](https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-build-type) という設定項目があり、この値は自動的に推論され、デフォルトでは `Simple` か `Custom` のどちらかになります。(他にも `Configure`, `Make` などもあります。詳しくはドキュメント参照)

値に `Simple` が指定された場合、`Setup.hs` の内容は以下のものとして処理されます。

```haskell
import Distribution.Simple
main = defaultMain
```

そのため `Setup.hs` を削除しても問題なくビルド可能です。

### Custom

`build-type` の値を `Custom` にした場合、`Setup.hs` の内容を自由に書き換えることができます。

Cabal には [defaultMainWithHooks](https://www.stackage.org/haddock/lts-13.0/Cabal-2.4.1.0/Distribution-Simple.html#v:defaultMainWithHooks) という素晴らしい関数が用意されています。

この関数を利用することで、コマンドの前後に好きな処理を挟むことができます。(コマンドの処理の上書きも可能です)

詳しいドキュメントは [3.3.8. More complex packages](https://www.haskell.org/cabal/users-guide/developing-packages.html#more-complex-packages) を参照ください。

### UserHooks

[UserHooks](https://www.stackage.org/haddock/lts-13.0/Cabal-2.4.1.0/Distribution-Simple.html#t:UserHooks) 型はこんな感じで定義されてます。

```hs
data UserHooks = UserHooks {
    runTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO (),
    readDesc :: IO (Maybe GenericPackageDescription),
    hookedPreProcessors :: [ PPSuffixHandler ],
    hookedPrograms :: [Program],

    preConf  :: Args -> ConfigFlags -> IO HookedBuildInfo,
    confHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo,
    postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    preBuild  :: Args -> BuildFlags -> IO HookedBuildInfo,
    buildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO (),
    postBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    preRepl  :: Args -> ReplFlags -> IO HookedBuildInfo,
    replHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> ReplFlags -> [String] -> IO (),
    postRepl :: Args -> ReplFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    preClean  :: Args -> CleanFlags -> IO HookedBuildInfo,
    cleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO (),
    postClean :: Args -> CleanFlags -> PackageDescription -> () -> IO (),

    preCopy  :: Args -> CopyFlags -> IO HookedBuildInfo,
    copyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO (),
    postCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    preInst  :: Args -> InstallFlags -> IO HookedBuildInfo,
    instHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO (),
    postInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    preSDist  :: Args -> SDistFlags -> IO HookedBuildInfo,
    sDistHook :: PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> SDistFlags -> IO (),
    postSDist :: Args -> SDistFlags -> PackageDescription -> Maybe LocalBuildInfo -> IO (),

    preReg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
    regHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO (),
    postReg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    preUnreg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
    unregHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO (),
    postUnreg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    preHscolour  :: Args -> HscolourFlags -> IO HookedBuildInfo,
    hscolourHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HscolourFlags -> IO (),
    postHscolour :: Args -> HscolourFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    preDoctest  :: Args -> DoctestFlags -> IO HookedBuildInfo,
    doctestHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> DoctestFlags -> IO (),
    postDoctest :: Args -> DoctestFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    preHaddock  :: Args -> HaddockFlags -> IO HookedBuildInfo,
    haddockHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO (),
    postHaddock :: Args -> HaddockFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    preTest :: Args -> TestFlags -> IO HookedBuildInfo,
    testHook :: Args -> PackageDescription -> LocalBuildInfo -> UserHooks -> TestFlags -> IO (),
    postTest :: Args -> TestFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    preBench :: Args -> BenchmarkFlags -> IO HookedBuildInfo,
    benchHook :: Args -> PackageDescription -> LocalBuildInfo -> UserHooks -> BenchmarkFlags -> IO (),
    postBench :: Args -> BenchmarkFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  }
```

これだけあれば、やりたいことはほとんど出来そうですね。

## 実装

フックする場所はたくさんあるので問題無さそうです。

しかし、これだけあると、どこに hook すれば良いのか？ということになるのですが、処理的には `package.yaml` から `.cabal` ファイルを生成したいので `.cabal` ファイルを読み込む手前で差し込む必要がありそうです。

適当に cabal のコードを読んでいると、どうやら [establishProjectBaseContext](https://github.com/haskell/cabal/blob/e15d87d542b4b23983aed3d54e0b42585257f453/cabal-install/Distribution/Client/CmdBuild.hs#L119) という関数が `.cabal` ファイルを読み込んでコンテキストを作っているっぽいことがわかりました。

なので、`preBuild` にフックすれば良さそうです。

こんな感じで完成しました。

```hs
module Main (main) where

import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo)
import Hpack

main :: IO ()
main = do
  let myHook = simpleUserHooks {
    preBuild = pbHpack
  }
  defaultMainWithHooks myHook

pbHpack :: Args -> BuildFlags -> IO HookedBuildInfo
pbHpack _ _ = do
  hpack Verbose (defaultOptions { optionsForce = Force })
  return (Nothing, [])
```

[hpack](https://www.stackage.org/package/hpack) パッケージの関数をそのまま使ってます。

今回は他に何もしないので `return (Nothing, [])` で大丈夫でした。(たぶん)

最後に忘れてはいけないのが、`custom-setup` の設定です。

`Setup.hs` で利用する依存関係などは `library` や `executables`, `tests` と同様に `custom-setup` を `package.yaml` に追記します。

```yaml
custom-setup:
  dependencies:
    - base
    - Cabal
    - hpack
```

これで完成です。

最初の一度だけは `hpack` を使って `cabal` ファイルを生成する必要がありますが、それ以降は必要ありません。

```shell
$ curl -sSL https://github.com/sol/hpack/raw/master/get-hpack.sh | bash
$ hpack package.yaml
$ cabal new-build
```

## 失敗したこと

最初はフックさせずにこんな感じですぐに実装できるものだと思ってましたが、

```hs
module Main (main) where

import Distribution.Simple
import Hpack

main :: IO ()
main = do
  hpack Verbose (defaultOptions { optionsForce = Force })
  defaultMain myHook
```

これだと、あんまり上手くいきません。

`package.yaml` を更新しても実行されなかったり、期待するタイミングで `.cabal` ファイルが生成されていないっぽい感じでした。

## まとめ

`stack` も `cabal` も両方とも良いビルドツールだと思います。

もうちょっとしたら、`cabal` も同様にオススメしていこうと思います。