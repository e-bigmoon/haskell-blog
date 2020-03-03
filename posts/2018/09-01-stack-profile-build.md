---
title: stack でアプリケーションのバックトレースを取得する (デバッグ)
author: Shinya Yamaguchi
tags: bigmoon, stack
updated: 2018/09/02
---

## はじめに

Haskell のデバッグ手法については、以下の記事がとても詳しく参考になります。素晴らしい記事です。

- [Haskell でのデバッグ手法あれこれ](https://blog.miz-ar.info/2018/01/debugging-haskell-program)

今回はHaskellアプリケーションをデバックする際に、バックトレースのとても簡単な取得方法があったのでご紹介したいと思います。

まとめると以下の2行です。

```shell
$ stack build --profile
$ stack exec -- <exe_name> +RTS -xc
```

<!--more-->

## バックトレースの取得方法

まず、バックトレースを取得するために [GHC.Stack](https://www.stackage.org/haddock/lts-12.8/base-4.11.1.0/GHC-Stack.html) で定義されている [HasCallStack](https://www.stackage.org/haddock/lts-12.8/base-4.11.1.0/GHC-Stack.html#t:HasCallStack) をクラス制約として追加します。

```hs
module Lib where

f1 :: IO ()
f1 = f2

f2 :: IO ()
f2 = f3

f3 :: IO ()
f3 = error "f3"
```

上記の例を雛形として、**HasCallStack** を追加するとどうなるのか確認していきましょう。

`Main.hs` はこんな感じで **f1** を呼び出します。

```hs
module Main (main) where

import Lib

main :: IO ()
main = f1
```

**HasCallStack** を追加しない場合は以下のような出力となります。

```shell
$ stack build
...
$ stack exec example
example: f3
CallStack (from HasCallStack):
  error, called at src/Lib.hs:10:6 in backtrace-example-0.1.0.0-hgO68xdg85BUcmGsdFId:Lib
```

**f3** でエラーが発生したことはわかりますが、それ以外は何もわかりません。

### f1, f2, f3 に HasCallStack を追加した場合

```hs
module Lib where

import GHC.Stack

f1 :: HasCallStack => IO ()
f1 = f2

f2 :: HasCallStack => IO ()
f2 = f3

f3 :: HasCallStack => IO ()
f3 = error "f3"
```

ビルドしてみます。

```shell
$ stack build
...
$ stack exec example
example: f3
CallStack (from HasCallStack):
  error, called at src/Lib.hs:12:6 in backtrace-example-0.1.0.0-hgO68xdg85BUcmGsdFId:Lib
  f3, called at src/Lib.hs:9:6 in backtrace-example-0.1.0.0-hgO68xdg85BUcmGsdFId:Lib
  f2, called at src/Lib.hs:6:6 in backtrace-example-0.1.0.0-hgO68xdg85BUcmGsdFId:Lib
  f1, called at app/Main.hs:6:8 in main:Main
```

ちゃんと呼び出しの関係が `error` -> `f3` -> `f2` -> `f1` と表示されていますね！

### f2, f3 にのみ HasCallStack を追加する

では、ここで `f1` の **HasCallStack** 制約を取り除くとどうなるか確認してみます。

```hs
module Lib where

import GHC.Stack

f1 :: IO ()
f1 = f2

f2 :: HasCallStack => IO ()
f2 = f3

f3 :: HasCallStack => IO ()
f3 = error "f3"
```

実行してみます。

```shell
$ stack build
...
$ stack exec example
example: f3
CallStack (from HasCallStack):
  error, called at src/Lib.hs:12:6 in backtrace-example-0.1.0.0-hgO68xdg85BUcmGsdFId:Lib
  f3, called at src/Lib.hs:9:6 in backtrace-example-0.1.0.0-hgO68xdg85BUcmGsdFId:Lib
  f2, called at src/Lib.hs:6:6 in backtrace-example-0.1.0.0-hgO68xdg85BUcmGsdFId:Lib
```

**f1** の **HasCallStack** 制約を取り除いた影響により、先程出力されていた `f1, called at app/Main.hs:6:8 in main:Main` が出なくなりました。

しかし、依然として **f2** と **f3** のバックトレースは取得できています。そのため、本当に全てのバックトレースを取得したい場合は全ての関数に **HasCallStack** 制約を追加しなけばなりません。

### f1 と f3 にのみ HasCallStack 制約を追加する

次に中間の **f2** の **HasCallStack** 制約を取り除いてみましょう。

```hs
module Lib where

import GHC.Stack

f1 :: HasCallStack => IO ()
f1 = f2

f2 :: IO ()
f2 = f3

f3 :: HasCallStack => IO ()
f3 = error "f3"
```

実行してみます。

```shell
$ stack build
...
$ stack exec example
example: f3
CallStack (from HasCallStack):
  error, called at src/Lib.hs:12:6 in backtrace-example-0.1.0.0-hgO68xdg85BUcmGsdFId:Lib
  f3, called at src/Lib.hs:9:6 in backtrace-example-0.1.0.0-hgO68xdg85BUcmGsdFId:Lib
```

今度は **f2** と **f1** の両方の情報がが抜け落ちてしまいました。**f2** で **HasCallStack** の伝搬がストップしてしまったということです。

### f1 と f2 のみに HasCallStack を追加する

```hs
module Lib where

import GHC.Stack

f1 :: HasCallStack => IO ()
f1 = f2

f2 :: HasCallStack => IO ()
f2 = f3

f3 :: IO ()
f3 = error "f3"
```

```shell
$ stack build
$ stack exec example
example: f3
CallStack (from HasCallStack):
  error, called at src/Lib.hs:12:6 in backtrace-example-0.1.0.0-hgO68xdg85BUcmGsdFId:Lib
...
```

予想通り **f1**, **f2**, **f3** の全ての情報が途絶えましたね・・・。

## HasCallStack を省略したままバックトレースを取りたい

先程の結果を見たとおり、正確な情報を取得するためには全ての関数に **HasCallStack** 制約を追加する必要がありそうです。

しかしながら、規模が大きくなってくるとそんなの不可能ですよね。

そんな時には `stack` の `--profile` オプションと `+RTS -xc` オプションを利用します。(`--profile` オプションは **ghc** のオプションを良い感じに追加してくれます)

```hs
module Lib where

f1 :: IO ()
f1 = f2

f2 :: IO ()
f2 = f3

f3 :: IO ()
f3 = error "f3"
```

上記のような素のコードに対しても、同様にバックトレースが取得できるようになります。

```shell
$ stack build --profile
...
$ stack exec -- example +RTS -xc
*** Exception (reporting due to +RTS -xc): (THUNK_2_0), stack trace:
  Lib.f3,
  called from Lib.CAF:f3
  --> evaluated by: Lib.CAF:f2
  --> evaluated by: Lib.CAF:f1
  --> evaluated by: Main.CAF:main
example: f3
CallStack (from HasCallStack):
  error, called at src/Lib.hs:10:6 in backtrace-example-0.1.0.0-hgO68xdg85BUcmGsdFId:Lib
CallStack (from -prof):
  Lib.f3 (src/Lib.hs:10:1-15)
  Lib.CAF:f3 (src/Lib.hs:10:1-2)
```

なんか沢山表示されますが、`reporting due to +RTS -xc` の部分を見ると、しっかりとバックトレースが取得できています。

## まとめ

- **HasCallStack** を使うとバックトレースが取得できる
- `stack build --profile` でビルドする
- `stack exec -- <exe> +RTS -xc` を使えば、明示的に **HasCallStack** を追加しなくても、バックトレースが取得できる
- アプリケーションのコードに対して有効

参考

- [Haskell でのデバッグ手法あれこれ](https://blog.miz-ar.info/2018/01/debugging-haskell-program/#HasCallStack)
- [Debugging -- stack document](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md#debugging)
- [-xc option -- GHC Users Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#rts-flag--xc)

以上です。
