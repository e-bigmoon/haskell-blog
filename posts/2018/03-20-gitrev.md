---
title: アプリケーションのバージョンに Git の情報を出してみよう！
author: Shinya Yamaguchi
tags: bigmoon, package
---

## はじめに

今回は [gitrev](https://www.stackage.org/package/gitrev) パッケージと [optparse-simple](https://www.stackage.org/package/optparse-simple) パッケージを使ってアプリケーションに `Git` の情報を含めてみようと思います！

利用者の多いアプリケーションだと、バグ報告時にどのコミットでビルドしたものなのか知りたい場合に便利です。

表示されるバージョン情報はこんな感じになります。

```hs
# gitrev の例
$ ./Main.hs
Main.hs: [panic master@3a0bd17fdfb8a3e334292a560280e8e0791e941c (Tue Mar 20 02:00:17 2018 +0900) (1 commits in HEAD)]

# optparse-simple の例
$ stack exec -- example-version-exe
Version 0.1.0.0, Git revision 341e785b02c4c599f64b922b4aa9cfff3c006945
```

以下のアプリケーション等でも利用されているように、実用度はとても高めです。

- [stack](https://github.com/commercialhaskell/stack)
- [hie](https://github.com/haskell/haskell-ide-engine)
- [liquidhaskell](https://github.com/ucsd-progsys/liquidhaskell)

また、実装コストもそれほどかからないため、オススメです！

<!--more-->

## gitrev パッケージ

[gitrev](https://www.stackage.org/haddock/lts-11.1/gitrev-1.3.1/Development-GitRev.html) パッケージは `Template Haskell` の機能を使ってコンパイル時に以下の情報を取得可能です。

- ブランチ名および、タグ名
- コミット数
- コミット日
- コミットの describe
- コミットのハッシュ

そのため、表示する情報を自分の好きなようにカスタマイズしたい場合に便利でしょう。

### サンプルアプリケーション

以下のコードは `Hackage` に載っているコードです。

```hs
#!/usr/bin/env stack
-- stack --resolver lts-11.1 script
{-# LANGUAGE TemplateHaskell #-}
import Development.GitRev

panic :: String -> a
panic msg = error panicMsg
  where panicMsg =
          concat [ "[panic ", $(gitBranch), "@", $(gitHash)
                 , " (", $(gitCommitDate), ")"
                 , " (", $(gitCommitCount), " commits in HEAD)"
                 , dirty, "] ", msg ]
        dirty | $(gitDirty) = " (uncommitted files present)"
              | otherwise   = ""

main = panic "oh no!"
```

実際に実行してみましょう。

```sh
$ tree .
.
└── Main.hs

0 directories, 1 file

$ chmod u+x Main.hs
$ ./Main.hs
Main.hs: [panic UNKNOWN@UNKNOWN (UNKNOWN) (UNKNOWN commits in HEAD)] oh no!
CallStack (from HasCallStack):
  error, called at /home/bm12/Desktop/gitrev-sample/Main.hs:7:13 in main:Main
```

現状は `git` のコミットが無いため、全て `UNKNOWN` として表示されています。

実際に `git` リポジトリを作ってコミットしてみましょう。

```sh
$ git init
Initialized empty Git repository in /home/bm12/Desktop/gitrev-sample/.git/

$ git add -A
$ git commit -m "TEST"
[master (root-commit) 3a0bd17] TEST
 1 file changed, 16 insertions(+)
 create mode 100755 Main.hs

$ ./Main.hs
Main.hs: [panic master@3a0bd17fdfb8a3e334292a560280e8e0791e941c (Tue Mar 20 02:00:17 2018 +0900) (1 commits in HEAD)] oh no!
```

こんな感じで `git` の情報を自由に組み合わせることができます。

## optparse-simple パッケージ

あまり書式を気にせず、定形で良い場合はもっと簡単な方法があります。

それは `optparse-simple` パッケージの [simpleVersion](https://www.stackage.org/haddock/lts-11.1/optparse-simple-0.1.0/Options-Applicative-Simple.html#v:simpleVersion) を使う方法です。

`simpleVersion` では `git` の情報だけでなく、アプリケーションのバージョンも一緒に表示することができます。

### simpleVersion

[simpleVersion](https://www.stackage.org/haddock/lts-11.1/optparse-simple-0.1.0/src/Options.Applicative.Simple.html#simpleVersion) の定義は以下のようになっています。

```hs
simpleVersion :: Version -> Q Exp
simpleVersion version =
  [|concat (["Version "
           ,$(TH.lift $ showVersion version)
           ] ++
           if $gitHash == ("UNKNOWN" :: String)
             then []
             else
               [", Git revision "
               ,$gitHash
               ,if $gitDirty
                   then " (dirty)"
                   else ""
               ])|]
```

`$gitHash` や `$gitDirty` を見ればわかる通り、内部的に `gitrev` パッケージを利用して `git` の情報を取得しています。

また、第一引数の `Version` 型は `base` パッケージの [Data.Version](https://www.stackage.org/haddock/lts-11.1/base-4.10.1.0/Data-Version.html) で定義されている型です。

以下のように `makeVersion :: [Int] -> Version` 関数を使って `Version` 型の値を作ることができます。

```hs
{-# LANGUAGE TemplateHaskell   #-}

import           Options.Applicative.Simple (simpleVersion)
import           Data.Version (makeVersion)

main :: IO ()
main = putStrLn $(simpleVersion $ makeVersion [100,0,0,0])
```

適当にプロジェクトを作って、上記の内容を実行してみましょう。

```sh
$ stack exec example-exe 
Version 100.0.0.0
```

まだ `git` で管理していないため、バージョン情報のみが表示されます。

では、先ほどの例と同様に `git` リポジトリを作ってコミットした結果を見てみましょう。

```sh
$ git init
Initialized empty Git repository in /home/bm12/Desktop/gitrev-sample/.git/

$ git add -A
$ git commit -m "TEST"
[master (root-commit) 3a0bd17] TEST
 1 file changed, 16 insertions(+)
 create mode 100755 Main.hs

$ stack clean
$ stack build
$ stack exec example-exe 
Version 100.0.0.0, Git revision e106394f7fdded0c9908cbf8edc87c5d5d5b4309
```

このようにちゃんとリビジョンが表示されるようになりました。

### アプリケーションのバージョンを自動的に更新する

アプリケーションのリリースごとに `makeVersion` を使って更新する作業はとても面倒ですし、いつか間違えてしまうかもしれません。

これを解決するためには `.cabal` ファイルからバージョン情報を自動取得して `simpleVersion` 関数に渡してあげるようにすれば良いのです。

```hs
{-# LANGUAGE TemplateHaskell   #-}

import           Options.Applicative.Simple (simpleVersion)
import qualified Paths_XXXX               as Meta

main :: IO ()
main = putStrLn $(simpleVersion Meta.version)
```

`Paths_XXXX` の `XXXX` はアプリケーション名 (`cabal` ファイルの `name` の値) を指定します。これで `cabal` ファイルのバージョン情報を直接取得できるようになります。

表示される結果はこんな感じです。

```sh
$ stack exec -- example-version-exe
Version 0.1.0.0, Git revision 341e785b02c4c599f64b922b4aa9cfff3c006945
```

めちゃめちゃ簡単なのでオススメの方法です。

## まとめ

自分のアプリケーションがとてもカッコよくなるので是非ためしてみてください！

以上です。
