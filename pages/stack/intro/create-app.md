---
title: アプリケーションの作成
prev: ./exec-prg.html
---

ここまででライブラリ (`Minfree.hs`) の作成が終わりました。

ここからは、そのライブラリを使って動く実行ファイルを作ってみましょう。

`app/Main.hs` の内容を以下のように書き換えましょう。

```haskell:app/Main.hs
module Main (main) where

import System.Environment (getArgs)
import Minfree (minfree)

main :: IO ()
main = do
  [xs] <- getArgs
  print $ minfree $ read xs
```

このコマンドを実行するためには次のようにします。

```shell-session
$ stack exec -- PFAD-exe [0,1,2,3,5,6]
4
```

デフォルトの `PFAD-exe` という名前が気に入らない人は `package.yaml` の `executables` を変更しましょう。

```yaml:package.yaml
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

ビルドしなおすと、指定したコマンド名で実行できるはずです。

```shell-session
$ stack exec -- minfree [0,1,2,3,4,7]
5
```

##### minfree2 の作成

アプリケーションは複数作ることができます。例えば今回 `minfree` 関数とそれを改良した `minfree'` 関数がありました。別のアプリケーションとして `minfree'` を使った `minfree2` を作ってみましょう。

まずは `package.yaml` の `executables` に新しいアプリケーションの内容を追記します。

```yaml:package.yaml
executables:
  minfree:
    main:                app/Main.hs
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
  # ここから下の行を追記しました
  minfree2:
    main:                app/Main2.hs
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
```

先ほどのプログラムとほぼ同じですが、以下のように `app2/Main.hs` を作ってみましょう。

```hs:app2/Main.hs
module Main (main) where

import System.Environment (getArgs)
import Minfree (minfree')

main :: IO ()
main = do
  [xs] <- getArgs
  print $ minfree' $ read xs
```

実行してみます。

```shell-session
$ stack exec -- minfree2 [1,2,3,4,5]
0
```
