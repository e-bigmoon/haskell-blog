---
title: アプリケーションの作成
date: 2018/05/05
prev: ./exec-prg.html
---

## アプリケーションの作成

ここまででライブラリ (**Minfree.hs**) の作成が終わりました。

ここからは、そのライブラリを使って動く実行ファイルを作ってみましょう。

**app/Main.hs** の内容を以下のように書き換えましょう。

```app/Main.hs
module Main (main) where

import System.Environment (getArgs)
import Minfree (minfree)

main :: IO ()
main = do
  [xs] <- getArgs
  print $ minfree $ read xs
```

ファイルを変更したので、まずはビルドしましょう。

```shell
$ stack build
```

次に、アプリケーションを実行してみましょう。

```shell
$ stack exec -- PFAD-exe [0,1,2,3,5,6]
4
```

`--` で区切ることで、アプリケーションに引数を渡すことができます。

### アプリケーション名の変更

デフォルトの **PFAD-exe** という名前が気に入らない人は **package.yaml** の **executables** を変更しましょう。

今回は **PFAD-exe** から **minfree** に変更しました。

```yaml
executables:
  minfree: # ここを変更します
    main:                app/Main.hs
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
```

ビルドし直せば、指定したコマンド名で実行できるはずです。

```shell
$ stack build
$ stack exec -- minfree [0,1,2,3,4,7]
5
```

## アプリケーションを複数作成する

1つのプロジェクトでアプリケーションを複数作ることもできます。

例えば今回 **minfree** 関数とそれを改良した **minfree'** 関数がありました。

別のアプリケーションとして **minfree'** を利用するアプリケーション **minfree2** を作ってみましょう。

まずは **package.yaml** の **executables** に新しいアプリケーションの内容を追記します。

```yaml
executables:
  minfree:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
  # ここから下の行を追記しました
  minfree2:
    main:                minfree2/Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
```

今回は **app/minfree2/Main.hs** にアプリケーションのコードを書きます。(**minfree2** というディレクトリを新たに作成する点に注意してください)

先ほどのプログラムとほぼ同じですが、以下のように `app/minfree2/Main.hs` を作ってみましょう。

```hs
module Main (main) where

import System.Environment (getArgs)
import Minfree (minfree')

main :: IO ()
main = do
  [xs] <- getArgs
  print $ minfree' $ read xs
```

実行してみます。

```shell
$ stack exec -- minfree2 [0,1,2,3,4,7]
5

$ stack exec -- minfree [0,1,2,3,4,7]
5
```
