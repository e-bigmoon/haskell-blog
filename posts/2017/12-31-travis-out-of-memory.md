---
title: travis-ci の初回ビルドで OUT OF MEMORY が出た時の対処法
author: Shinya Yamaguchi
tags: bigmoon, travis, stack
updated: 2018/03/14
---

## はじめに

`travis-ci` の初回ビルド時に以下のようなエラーメッセージが表示されてビルドに失敗してしまうことがあります。

```shell
The command "stack --no-terminal test --only-dependencies" failed and exited with 1 during .

Your build has been stopped.
```

結論から言えば、初回ビルド時はキャッシュが働かないため、多くの依存関係をビルドする必要があり、その過程でメモリ不足になってしまっていました。

この問題をどうすれば解決できるか、メモ程度に残しておこうと思います。

<!--more-->

## .travis.yml

今回は以下のような `.travis.yml` を用意しました。

```yaml
{
  "sudo": false,
  "language": "generic",
  "cache": {
    "directories": [
      "$HOME/.stack/",
      "$HOME/.local/bin/",
      ".stack-work/"
    ]
  },
  "before_install": [
    "mkdir -p ~/.local/bin",
    "export PATH=$HOME/.local/bin:$PATH",
    "travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'"
  ],
  "install": [
    "stack --no-terminal test --only-dependencies"
  ],
  "group": "stable",
  "dist": "trusty",
  "os": "linux"
}
```

問題の行は `stack --no-terminal test --only-dependencies` です。


この記述は[公式ドキュメント](https://github.com/commercialhaskell/stack/blob/master/doc/travis_ci.md)の[The simple Travis configuration](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/travis-simple.yml) として紹介されています。

基本的には特に気にしなくても良いのですが `Hakyll` などの依存関係が多いプロジェクトでは `OUT OF MEMORY` が発生することがあります。

## エラーメッセージ

- 実際の[ログ](https://travis-ci.org/wataru86/haskell-blog/builds/323115071)

ちょっと長いですが、エラー部分のログを載せます。

```shell
regex-tdfa-1.2.2: copy/register
aeson-1.2.3.0: copy/register
JuicyPixels-3.2.9.1: copy/register
--  While building custom Setup.hs for package Cabal-2.0.1.1 using:
      /home/travis/.stack/setup-exe-cache/x86_64-linux/Cabal-simple_mPHDZzAJ_2.0.1.0_ghc-8.2.2 --builddir=.stack-work/dist/x86_64-linux/Cabal-2.0.1.0 build --ghc-options " -ddump-hi -ddump-to-file -fdiagnostics-color=always"
    Process exited with code: ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)
    Logs have been written to: /home/travis/build/wataru86/haskell-blog/.stack-work/logs/Cabal-2.0.1.1.log
    Configuring Cabal-2.0.1.1...
    Preprocessing library for Cabal-2.0.1.1..
    Building library for Cabal-2.0.1.1..
    [  1 of 168] Compiling Distribution.Compat.Binary ( Distribution/Compat/Binary.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/Binary.o )
    [  2 of 168] Compiling Distribution.Compat.Exception ( Distribution/Compat/Exception.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/Exception.o )
    [  3 of 168] Compiling Distribution.Compat.Internal.TempFile ( Distribution/Compat/Internal/TempFile.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/Internal/TempFile.o )
    [  4 of 168] Compiling Distribution.Compat.Map.Strict ( Distribution/Compat/Map/Strict.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/Map/Strict.o )
    [  5 of 168] Compiling Distribution.Compat.MonadFail ( Distribution/Compat/MonadFail.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/MonadFail.o )
    [  6 of 168] Compiling Distribution.Compat.Semigroup ( Distribution/Compat/Semigroup.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/Semigroup.o )
    [  7 of 168] Compiling Distribution.Compat.Stack ( Distribution/Compat/Stack.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/Stack.o )
    [  8 of 168] Compiling Distribution.Compat.Prelude ( Distribution/Compat/Prelude.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/Prelude.o )
    [  9 of 168] Compiling Distribution.Compat.SnocList ( Distribution/Compat/SnocList.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/SnocList.o )
    [ 10 of 168] Compiling Distribution.Compat.ReadP ( Distribution/Compat/ReadP.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/ReadP.o )
    [ 11 of 168] Compiling Distribution.Compat.Prelude.Internal ( Distribution/Compat/Prelude/Internal.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/Prelude/Internal.o )
    [ 12 of 168] Compiling Distribution.Compat.Graph ( Distribution/Compat/Graph.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/Graph.o )
    [ 13 of 168] Compiling Distribution.Compat.GetShortPathName ( Distribution/Compat/GetShortPathName.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/GetShortPathName.o )
    [ 14 of 168] Compiling Distribution.Compat.DList ( Distribution/Compat/DList.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/DList.o )
    [ 15 of 168] Compiling Distribution.Compat.CopyFile ( Distribution/Compat/CopyFile.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/CopyFile.o )
    [ 16 of 168] Compiling Distribution.Compat.Environment ( Distribution/Compat/Environment.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/Environment.o )
    [ 17 of 168] Compiling Distribution.Compat.CreatePipe ( Distribution/Compat/CreatePipe.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compat/CreatePipe.o )
    [ 18 of 168] Compiling Distribution.GetOpt ( Distribution/GetOpt.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/GetOpt.o )
    [ 19 of 168] Compiling Distribution.Lex ( Distribution/Lex.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Lex.o )
    [ 20 of 168] Compiling Distribution.PackageDescription.Utils ( Distribution/PackageDescription/Utils.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/PackageDescription/Utils.o )
    [ 21 of 168] Compiling Distribution.ReadE ( Distribution/ReadE.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/ReadE.o )
    [ 22 of 168] Compiling Distribution.Simple.CCompiler ( Distribution/Simple/CCompiler.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/CCompiler.o )
    [ 23 of 168] Compiling Distribution.Simple.PreProcess.Unlit ( Distribution/Simple/PreProcess/Unlit.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/PreProcess/Unlit.o )
    [ 24 of 168] Compiling Distribution.Simple.Program.Internal ( Distribution/Simple/Program/Internal.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Program/Internal.o )
    [ 25 of 168] Compiling Distribution.TestSuite ( Distribution/TestSuite.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/TestSuite.o )
    [ 26 of 168] Compiling Distribution.Text ( Distribution/Text.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Text.o )
    [ 27 of 168] Compiling Distribution.System ( Distribution/System.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/System.o )
    [ 28 of 168] Compiling Distribution.Types.BuildType ( Distribution/Types/BuildType.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/BuildType.o )
    [ 29 of 168] Compiling Distribution.Types.Condition ( Distribution/Types/Condition.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/Condition.o )
    [ 30 of 168] Compiling Distribution.Types.CondTree ( Distribution/Types/CondTree.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/CondTree.o )
    [ 31 of 168] Compiling Distribution.Types.ExecutableScope ( Distribution/Types/ExecutableScope.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ExecutableScope.o )
    [ 32 of 168] Compiling Distribution.Types.ForeignLibOption ( Distribution/Types/ForeignLibOption.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ForeignLibOption.o )
    [ 33 of 168] Compiling Distribution.Types.ForeignLibType ( Distribution/Types/ForeignLibType.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ForeignLibType.o )
    [ 34 of 168] Compiling Distribution.Types.SourceRepo ( Distribution/Types/SourceRepo.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/SourceRepo.o )
    [ 35 of 168] Compiling Distribution.Utils.Base62 ( Distribution/Utils/Base62.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Utils/Base62.o )
    [ 36 of 168] Compiling Distribution.Utils.MapAccum ( Distribution/Utils/MapAccum.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Utils/MapAccum.o )
    [ 37 of 168] Compiling Distribution.Utils.Progress ( Distribution/Utils/Progress.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Utils/Progress.o )
    [ 38 of 168] Compiling Distribution.Utils.String ( Distribution/Utils/String.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Utils/String.o )
    [ 39 of 168] Compiling Distribution.Utils.ShortText ( Distribution/Utils/ShortText.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Utils/ShortText.o )
    [ 40 of 168] Compiling Distribution.Types.PkgconfigName ( Distribution/Types/PkgconfigName.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/PkgconfigName.o )
    [ 41 of 168] Compiling Distribution.Types.ComponentId ( Distribution/Types/ComponentId.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ComponentId.o )
    [ 42 of 168] Compiling Distribution.Types.AbiHash ( Distribution/Types/AbiHash.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/AbiHash.o )
    [ 43 of 168] Compiling Distribution.ModuleName ( Distribution/ModuleName.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/ModuleName.o )
    [ 44 of 168] Compiling Distribution.Types.ModuleRenaming ( Distribution/Types/ModuleRenaming.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ModuleRenaming.o )
    [ 45 of 168] Compiling Distribution.Types.IncludeRenaming ( Distribution/Types/IncludeRenaming.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/IncludeRenaming.o )
    [ 46 of 168] Compiling Distribution.Utils.Generic ( Distribution/Utils/Generic.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Utils/Generic.o )
    [ 47 of 168] Compiling Distribution.Utils.UnionFind ( Distribution/Utils/UnionFind.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Utils/UnionFind.o )
    [ 48 of 168] Compiling Distribution.Verbosity ( Distribution/Verbosity.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Verbosity.o )
    [ 49 of 168] Compiling Distribution.Version ( Distribution/Version.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Version.o )
    [ 50 of 168] Compiling Distribution.Types.TestType ( Distribution/Types/TestType.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/TestType.o )
    [ 51 of 168] Compiling Distribution.Types.TestSuiteInterface ( Distribution/Types/TestSuiteInterface.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/TestSuiteInterface.o )
    [ 52 of 168] Compiling Distribution.Types.PkgconfigDependency ( Distribution/Types/PkgconfigDependency.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/PkgconfigDependency.o )
    [ 53 of 168] Compiling Distribution.Types.BenchmarkType ( Distribution/Types/BenchmarkType.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/BenchmarkType.o )
    [ 54 of 168] Compiling Distribution.Types.BenchmarkInterface ( Distribution/Types/BenchmarkInterface.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/BenchmarkInterface.o )
    [ 55 of 168] Compiling Distribution.License ( Distribution/License.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/License.o )
    [ 56 of 168] Compiling Language.Haskell.Extension ( Language/Haskell/Extension.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Language/Haskell/Extension.o )
    [ 57 of 168] Compiling Distribution.Compiler ( Distribution/Compiler.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Compiler.o )
    [ 58 of 168] Compiling Distribution.PrettyUtils ( Distribution/PrettyUtils.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/PrettyUtils.o )
    [ 59 of 168] Compiling Distribution.ParseUtils ( Distribution/ParseUtils.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/ParseUtils.o )
    [ 60 of 168] Compiling Distribution.Types.PackageName ( Distribution/Types/PackageName.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/PackageName.o )
    [ 61 of 168] Compiling Distribution.Types.UnqualComponentName ( Distribution/Types/UnqualComponentName.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/UnqualComponentName.o )
    [ 62 of 168] Compiling Distribution.Types.ComponentName ( Distribution/Types/ComponentName.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ComponentName.o )
    [ 63 of 168] Compiling Distribution.Types.PackageId ( Distribution/Types/PackageId.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/PackageId.o )
    [ 64 of 168] Compiling Distribution.Types.UnitId ( Distribution/Types/UnitId.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/UnitId.o )
    [ 65 of 168] Compiling Distribution.Types.Module ( Distribution/Types/Module.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/Module.o )
    [ 66 of 168] Compiling Distribution.Backpack ( Distribution/Backpack.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Backpack.o )
    [ 67 of 168] Compiling Distribution.Backpack.ModSubst ( Distribution/Backpack/ModSubst.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Backpack/ModSubst.o )
    [ 68 of 168] Compiling Distribution.Backpack.FullUnitId ( Distribution/Backpack/FullUnitId.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Backpack/FullUnitId.o )
    [ 69 of 168] Compiling Distribution.Types.ModuleReexport ( Distribution/Types/ModuleReexport.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ModuleReexport.o )
    [ 70 of 168] Compiling Distribution.Types.Mixin ( Distribution/Types/Mixin.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/Mixin.o )
    [ 71 of 168] Compiling Distribution.Types.ExeDependency ( Distribution/Types/ExeDependency.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ExeDependency.o )
    [ 72 of 168] Compiling Distribution.Types.Dependency ( Distribution/Types/Dependency.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/Dependency.o )
    [ 73 of 168] Compiling Distribution.Types.SetupBuildInfo ( Distribution/Types/SetupBuildInfo.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/SetupBuildInfo.o )
    [ 74 of 168] Compiling Distribution.Types.DependencyMap ( Distribution/Types/DependencyMap.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/DependencyMap.o )
    [ 75 of 168] Compiling Distribution.Simple.GHC.IPIConvert ( Distribution/Simple/GHC/IPIConvert.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/GHC/IPIConvert.o )
    [ 76 of 168] Compiling Distribution.Backpack.ModuleScope ( Distribution/Backpack/ModuleScope.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Backpack/ModuleScope.o )
    [ 77 of 168] Compiling Distribution.Types.MungedPackageName ( Distribution/Types/MungedPackageName.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/MungedPackageName.o )
    [ 78 of 168] Compiling Distribution.Types.MungedPackageId ( Distribution/Types/MungedPackageId.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/MungedPackageId.o )
    [ 79 of 168] Compiling Distribution.Package ( Distribution/Package.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Package.o )
    [ 80 of 168] Compiling Distribution.Types.AnnotatedId ( Distribution/Types/AnnotatedId.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/AnnotatedId.o )
    [ 81 of 168] Compiling Distribution.Types.ComponentInclude ( Distribution/Types/ComponentInclude.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ComponentInclude.o )
    [ 82 of 168] Compiling Distribution.Simple.InstallDirs ( Distribution/Simple/InstallDirs.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/InstallDirs.o )
    [ 83 of 168] Compiling Distribution.Types.LegacyExeDependency ( Distribution/Types/LegacyExeDependency.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/LegacyExeDependency.o )
    [ 84 of 168] Compiling Distribution.Types.BuildInfo ( Distribution/Types/BuildInfo.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/BuildInfo.o )
    [ 85 of 168] Compiling Distribution.Types.TestSuite ( Distribution/Types/TestSuite.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/TestSuite.o )
    [ 86 of 168] Compiling Distribution.Types.Library ( Distribution/Types/Library.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/Library.o )
    [ 87 of 168] Compiling Distribution.Types.HookedBuildInfo ( Distribution/Types/HookedBuildInfo.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/HookedBuildInfo.o )
    [ 88 of 168] Compiling Distribution.Types.ForeignLib ( Distribution/Types/ForeignLib.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ForeignLib.o )
    [ 89 of 168] Compiling Distribution.Types.Executable ( Distribution/Types/Executable.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/Executable.o )
    [ 90 of 168] Compiling Distribution.Types.Benchmark ( Distribution/Types/Benchmark.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/Benchmark.o )
    [ 91 of 168] Compiling Distribution.Types.Component ( Distribution/Types/Component.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/Component.o )
    [ 92 of 168] Compiling Distribution.Types.ComponentRequestedSpec ( Distribution/Types/ComponentRequestedSpec.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ComponentRequestedSpec.o )
    [ 93 of 168] Compiling Distribution.Types.PackageDescription ( Distribution/Types/PackageDescription.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/PackageDescription.o )
    [ 94 of 168] Compiling Distribution.Types.GenericPackageDescription ( Distribution/Types/GenericPackageDescription.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/GenericPackageDescription.o )
    [ 95 of 168] Compiling Distribution.PackageDescription ( Distribution/PackageDescription.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/PackageDescription.o )
    [ 96 of 168] Compiling Distribution.Simple.BuildToolDepends ( Distribution/Simple/BuildToolDepends.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/BuildToolDepends.o )
    [ 97 of 168] Compiling Distribution.InstalledPackageInfo ( Distribution/InstalledPackageInfo.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/InstalledPackageInfo.o )
    [ 98 of 168] Compiling Distribution.Types.ComponentLocalBuildInfo ( Distribution/Types/ComponentLocalBuildInfo.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/ComponentLocalBuildInfo.o )
    [ 99 of 168] Compiling Distribution.Types.TargetInfo ( Distribution/Types/TargetInfo.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Types/TargetInfo.o )
    [100 of 168] Compiling Distribution.Simple.GHC.IPI642 ( Distribution/Simple/GHC/IPI642.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/GHC/IPI642.o )
    [101 of 168] Compiling Distribution.Backpack.ModuleShape ( Distribution/Backpack/ModuleShape.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Backpack/ModuleShape.o )
    [102 of 168] Compiling Distribution.Backpack.UnifyM ( Distribution/Backpack/UnifyM.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Backpack/UnifyM.o )
    [103 of 168] Compiling Distribution.Backpack.MixLink ( Distribution/Backpack/MixLink.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Backpack/MixLink.o )
    [104 of 168] Compiling Distribution.Backpack.PreExistingComponent ( Distribution/Backpack/PreExistingComponent.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Backpack/PreExistingComponent.o )
    [105 of 168] Compiling Paths_Cabal      ( .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/autogen/Paths_Cabal.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Paths_Cabal.o )
    [106 of 168] Compiling Distribution.Simple.Utils ( Distribution/Simple/Utils.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Utils.o )
    [107 of 168] Compiling Distribution.Utils.NubList ( Distribution/Utils/NubList.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Utils/NubList.o )
    [108 of 168] Compiling Distribution.Utils.LogProgress ( Distribution/Utils/LogProgress.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Utils/LogProgress.o )
    [109 of 168] Compiling Distribution.Simple.Program.Find ( Distribution/Simple/Program/Find.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Program/Find.o )
    [110 of 168] Compiling Distribution.Simple.Program.Types ( Distribution/Simple/Program/Types.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Program/Types.o )
    [111 of 168] Compiling Distribution.Simple.Program.Run ( Distribution/Simple/Program/Run.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Program/Run.o )
    [112 of 168] Compiling Distribution.Simple.Program.Script ( Distribution/Simple/Program/Script.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Program/Script.o )
    [113 of 168] Compiling Distribution.Simple.Program.Ld ( Distribution/Simple/Program/Ld.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Program/Ld.o )
    [114 of 168] Compiling Distribution.Simple.Program.Hpc ( Distribution/Simple/Program/Hpc.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Program/Hpc.o )
    [115 of 168] Compiling Distribution.Simple.Program.Builtin ( Distribution/Simple/Program/Builtin.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Program/Builtin.o )
    [116 of 168] Compiling Distribution.Simple.Program.Db ( Distribution/Simple/Program/Db.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Program/Db.o )
    [117 of 168] Compiling Distribution.Simple.Program ( Distribution/Simple/Program.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Program.o )
    [118 of 168] Compiling Distribution.Simple.Program.Strip ( Distribution/Simple/Program/Strip.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Program/Strip.o )
    [119 of 168] Compiling Distribution.Simple.PackageIndex ( Distribution/Simple/PackageIndex.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/PackageIndex.o )
    [120 of 168] Compiling Distribution.Simple.Compiler ( Distribution/Simple/Compiler.hs, .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/Distribution/Simple/Compiler.o )
The command "stack --no-terminal build --only-dependencies" failed and exited with 1 during .
Your build has been stopped.
```

これ、一瞬見ただけだと良くわからないのですが、注目するのは以下の部分です。

```shell
While building custom Setup.hs for package Cabal-2.0.1.1 using:
      /home/travis/.stack/setup-exe-cache/x86_64-linux/Cabal-simple_mPHDZzAJ_2.0.1.0_ghc-8.2.2 --builddir=.stack-work/dist/x86_64-linux/Cabal-2.0.1.0 build --ghc-options " -ddump-hi -ddump-to-file -fdiagnostics-color=always"
    Process exited with code: ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)
```

以下の2点が今回のエラーに対処するために必要な情報になります。

- `While building custom Setup.hs for package Cabal-2.0.1.1 using`
- `ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)`

## 解決策その1 -j 1 オプションを利用する

`stack build` 時に `-j 1` オプションを指定することで並列ビルドを抑制することができます。(`stack` はデフォルトで並列ビルドが有効になってます)

```shell
$ stack --no-terminal build --only-dependencies -j 1
```

この方法で解決することもできますが、ビルド時間が増大するというデメリットもあります。

そうすると `travis-ci` の `50分制限` にひっかかり、やはりビルドが失敗してしまうのです。こんな感じのエラーが出ます。

```shell
The job exceeded the maximum time limit for jobs, and has been terminated.
```

## 解決策その2 問題のパッケージのみ先にビルドする

過去に同じような `issue` ([Building with stack on Travis CI - dependency compilation fails sometimes #859](https://github.com/commercialhaskell/stack/issues/859)) が立っており、そこに解決策がありました。

`-j 1` を全体に適用するとビルド時間がかかりすぎてしまうので、問題のパッケージのみ `-j 1` でビルドするという方法です。

先程のエラーメッセージから問題のパッケージを特定することができます。

```shell
While building custom Setup.hs for package Cabal-2.0.1.1 using
```

上記メッセージを読むと、悪いのは `Cabal` だということがわかります。

そのため、以下のように `.travis.yml` の設定を変更します。

```shell
stack --no-terminal build -j 1 Cabal
stack --no-terminal test --only-dependencies
```

この時に `stack --no-terminal build -j 1 Cabal --only-dependencies` としてしまうとパッケージはビルドされないのでご注意ください。

`.travis.yml` 全体はこうなります。

```yaml
{
  "sudo": false,
  "language": "generic",
  "cache": {
    "directories": [
      "$HOME/.stack/",
      "$HOME/.local/bin/",
      ".stack-work/"
    ]
  },
  "before_install": [
    "mkdir -p ~/.local/bin",
    "export PATH=$HOME/.local/bin:$PATH",
    "travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'"
  ],
  "install": [
    "stack --no-terminal build -j 1 Cabal",
    "stack --no-terminal test --only-dependencies"
  ],
  "group": "stable",
  "dist": "trusty",
  "os": "linux"
}
```

## まとめ

`travis` の `OUT OF MEMORY` エラーは噂には聞いていましたが、実際に自分が遭遇するのは初めてでした。

対処法としてはエラーの原因となるパッケージのみを `-j 1` オプションで先にビルドしてしまえば良いということがわかりました。
