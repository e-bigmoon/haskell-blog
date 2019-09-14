---
title: repl 環境の使い方
date: 2019/09/14
prev: ./create-lib.html
next: ./ghc.html
---

ここまでの内容を対話環境で実行したいと思います。

**stack ghci** と **stack repl** はただのエイリアスなので好きな方を使ってください。

## repl 環境

```shell
$ stack repl
Using main module: 1. Package `PFAD' component PFAD:exe:PFAD-exe with main-is file: /PFAD/app/Main.hs
PFAD> configure (lib + exe)
Configuring PFAD-0.1.0.0...
PFAD> initial-build-steps (lib + exe)
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: PFAD
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Main             ( /PFAD/app/Main.hs, interpreted )
[2 of 2] Compiling Minfree          ( /PFAD/src/Minfree.hs, interpreted )

/PFAD/src/Minfree.hs:3:1: error:
    Could not load module ‘Data.Array’
    It is a member of the hidden package ‘array-0.5.3.0’.
    You can run ‘:set -package array’ to expose it.
    (Note: this unloads all the modules in the current scope.)
    Use -v to see a list of the files searched for.
  |
3 | import Data.Array (Array, elems, accumArray, assocs)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

/PFAD/src/Minfree.hs:4:1: error:
    Could not load module ‘Data.Array.ST’
    It is a member of the hidden package ‘array-0.5.3.0’.
    You can run ‘:set -package array’ to expose it.
    (Note: this unloads all the modules in the current scope.)
    Use -v to see a list of the files searched for.
  |
4 | import Data.Array.ST (runSTArray, newArray, writeArray)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Failed, one module loaded.

<no location info>: error:
    Could not find module ‘Minfree’
    It is not a module in the current program, or in any known package.
Loaded GHCi configuration from /tmp/haskell-stack-ghci/35ac6387/ghci-script
```

少し長いですが、このエラーメッセージのうち特に重要な部分を抜き出してみましょう。

### エラーメッセージの読み方

エラーその1

```shell
/PFAD/src/Minfree.hs:3:1: error:
    Could not load module ‘Data.Array’
    It is a member of the hidden package ‘array-0.5.3.0’.
    You can run ‘:set -package array’ to expose it.
    (Note: this unloads all the modules in the current scope.)
    Use -v to see a list of the files searched for.
  |
3 | import Data.Array (Array, elems, accumArray, assocs)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

エラーその2

```shell
/PFAD/src/Minfree.hs:4:1: error:
    Could not load module ‘Data.Array.ST’
    It is a member of the hidden package ‘array-0.5.3.0’.
    You can run ‘:set -package array’ to expose it.
    (Note: this unloads all the modules in the current scope.)
    Use -v to see a list of the files searched for.
  |
4 | import Data.Array.ST (runSTArray, newArray, writeArray)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

このエラーメッセージから読み取って欲しい点は以下の3点です。

- エラーが発生したソースコードの場所
- エラーの原因 (**import** しようと思ったモジュールが見つからなかった)
- モジュールがどのパッケージで定義されているか

このエラーメッセージから読み取って欲しい点は以下の3点です。

エラーの内容 | エラーその1 | エラーその2
-----------|------------|-----------
エラーの発生箇所 | `PFAD/src/Minfree.hs:3:1` | `PFAD/src/Minfree.hs:4:1`
エラーの原因 | `Could not load module Data.Array` | `Could not load module Data.Array.ST`
モジュールが定義されているパッケージ | `array-0.5.3.0` | `array-0.5.3.0`

これらの情報から、壊れたビルドの直し方がわかります。

**ghci** の対話セッションから抜ける際は **:q** と入力します。

```shell
*Main> :q
Leaving GHCi.
```

## パッケージの指定方法

**stack ghci** コマンドを実行した場合 **base** パッケージが読み込まれます。そのため、それ以外のパッケージに含まれるモジュールを利用するためには明示的にパッケージを指定する必要があります。

パッケージを追加して起動する場合は `--package` オプションを使います。

```shell
$ stack ghci --package array
...
*Main Minfree>
```

別のやり方として **ghci** を起動した後に `:set -package` コマンドを実行する方法があります。

```shell
$ stack ghci
...
*Main> :set -package array
Prelude> import Data.Array

Prelude Data.Array>
```

この場合 **ghci** のセッションが完全に初期化されるため、プロンプトの表示が `*Main Minfree>` ではなく `Prelude>` になります。そのため、実際のプロジェクトではあまり使わないかもしれません。

## --no-load オプション

プロジェクト内で **stack ghci** を実行すると、プロジェクトのモジュールが読み込まれてしまいます。

場合によっては、初期状態の **ghci** を起動したい時があるかもしれません。その場合は `--no-load` オプションを使います。

```shell
$ stack ghci --no-load
Prelude>
```

## プロンプト文字列の変更

**stack ghci** を起動した時のプロンプトの表示名は `Prelude>` になっています。(何かモジュールを読み込んだ場合は、そのモジュール名が表示されます)

```shell
$ stack ghci --no-load
Prelude>
```

**ghci** で色々と試していると、モジュールをいくつも **import** したくなります。そうすると、プロンプトの表示名はインポートしたモジュール名が連なり、非常に使いづらいものになってしまいます。

```shell
Prelude> import Data.List
Prelude Data.List> import Data.Function 
Prelude Data.List Data.Function> import Control.Monad
Prelude Data.List Data.Function Control.Monad> import Data.Functor
Prelude Data.List Data.Function Control.Monad Data.Functor> 1+1
2
Prelude Data.List Data.Function Control.Monad Data.Functor>
```

この問題を解決するためには **:set prompto** コマンドを使います。

```shell
Prelude Data.List Data.Function Control.Monad Data.Functor> :set prompt "> "
> 1+1
2
> import Data.Bool
> :set prompt "λ "
λ 1+1
2
λ
```

こんな感じでスッキリさせることができます。

特に大きなプロジェクトを **stack ghci** で読み込むとプロンプトがひどいことになってしまので、このようにして対処すると良いでしょう。

## .ghci ファイル

プロンプトの表示を毎回自分で変更するのが面倒な人は `.ghci` ファイルを作成しましょう。

このファイルは **ghci** を起動した直後に実行するコマンドを並べたものです。プロジェクトのルートに作成します。

```shell
$ echo ':set prompt "λ "' > .ghci
```

実際に実行してみます。

```shell
stack ghci --no-load
PFAD> initial-build-steps (lib + exe)
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: PFAD
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /Users/gupi/Desktop/PFAD/.ghci
```

`Loaded GHCi configuration from /PFAD/.ghci` のような文字列が表示されれば、ちゃんと設定ファイルが読み込まれています。もし、設定ファイルの記述内容が間違っていた場合は `Some flags have not been recognized` のようなエラーが表示されるはずです。

```shell
# promptとtypoしてしまった場合のエラーメッセージ
$ stack ghci
...
Some flags have not been recognized: prompto, λ
Loaded GHCi configuration from /PFAD/.ghci
...
```
