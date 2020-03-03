---
title: アプリケーションの作成
published: 2017/12/24
updated: 2019/09/14
prev: ./extra-deps.html
next: ../doc/index.html
---

## 設定ファイルの内容確認

前の章で **stack.yaml** と **package.yaml** を色々と修正したので、ここからは以下の状態で進めていきます。

**stack.yaml** は以下の内容で進めていきます。

```yaml
resolver: lts-13.29
packages:
- .
```

また **package.yaml** の **dependencies** は以下のようになっています。

```yaml
dependencies:
- base >= 4.7 && < 5
- array
```

## アプリケーションの作成

ここまででライブラリ (**Minfree.hs**) の作成が終わりました。

ここからは、そのライブラリを使って動作する実行ファイルを作ってみましょう。

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
    main:                Main.hs
    source-dirs:         app
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

また、**stack 1.9.1** からは **stack run** というサブコマンドが実装されたため、`build -> exec` のステップが1ステップに簡略化されました。

```shell
$ stack run [0,1,2,3,4,7]
5
```

## アプリケーションを複数作成する

1つのプロジェクトでアプリケーションを複数作ることもできます。

例えば今回 **minfree** 関数とそれを改良した **minfree'** 関数がありました。

別のアプリケーションとして **minfree'** を利用するアプリケーション **fastMinfree** を作ってみましょう。

まずは、複数のアプリケーションをビルドできるように **source-dirs** を削除し、**main** の指定を **app/Main.hs** のようにします。

```yaml
executables:
  minfree:
    main: app/Main.hs    # この行を変更
                         # source-dirs: app は削除します
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
```

次に **package.yaml** の **executables** に新しいアプリケーションの内容を追記します。

```yaml
executables:
  minfree:
    main: app/Main.hs
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
  # ここから下の行を追記しました
  fastMinfree:
    main: app/FastMinfreeApp.hs
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
```

先ほどのプログラムとほぼ同じですが、以下のように `app/FastMinfreeApp.hs` を作ってみましょう。

```hs
-- app/FastMinfreeApp.hs
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
$ stack build
$ stack exec -- fastMinfree [1,2,3,4,5]
0
```

この場合も同様に **stack run** が使えますが、アプリケーションが2つになったため明示的にアプリケーション名を指定する必要があります。

```shell
$ stack run fastMinfree [1,2,3,4,5]
0
```

アプリケーション名が省略された場合は、**package.yaml** の **executables** に書かれている一番上のアプリケーションが実行されます。
