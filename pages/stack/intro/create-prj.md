---
title: プロジェクトの作成
---

```shell-session
$ stack new PFAD
$ cd PFAD
$ stack build

$ tree .
.
|-- ChangeLog.md
|-- LICENSE
|-- PFAD.cabal
|-- README.md
|-- Setup.hs
|-- app
|   `-- Main.hs
|-- package.yaml
|-- src
|   `-- Lib.hs
|-- stack.yaml
`-- test
    `-- Spec.hs
```

##### 準備

新たに作成するソースコードは `app`, `src`, `test` に以下にそれぞれ配置します。一応、以下のコマンドに対応するようにフォルダを分けることが多いです。

フォルダ | 関連するコマンド
--------|--------
app | stack exec
src | stack build
test | stack test

今回 `src/Lib.hs` は使わないので削除しておきましょう。

```shell-session
$ rm src/Lib.hs
```

そうすると当然 `Lib.hs` が無いのでコンパイルエラーになってしまいます。とりあえず現状は `src/Main.hs` を以下のように書き換えておきましょう。

```hs:app/Main.hs
module Main (main) where

main :: IO ()
main = undefined
```
