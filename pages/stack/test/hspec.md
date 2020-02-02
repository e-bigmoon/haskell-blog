---
title: テストフレームワーク (hspec)
date: 2019/09/14
prev: ./index.html
next: ./quickcheck.html
---

この章ではテストフレームワークの **hspec** を使ったテストの書き方を学習します。

## hspec-discover のインストール

[hspec-discover](https://hackage.haskell.org/package/hspec-discover) を利用することで、それぞれのソースファイルと一対一に対応した **Spec** ファイルを自動的に読み込んでテストしてくれるようになります。

```shell
$ stack install hspec-discover
$ hspec-discover
Usage: hspec-discover SRC CUR DST [--module-name=NAME]
```

## テストの作成

まずは **hspec** を使うために、**package.yaml** の **tests** に **hspec** パッケージを追記します。

```yaml
tests:
  PFAD-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
    - hspec # ここを追記
```

つぎに、**hspec-discover** を利用するため `test/Spec.hs` の内容を以下のように書き換えます。

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

### ファイルの命名規則

**hspec-discover** を利用する場合、テスト対象のファイルを自動的に見つけるために命名規則が決まっています。

**Spec** ファイルの命名規則は、**src/Minfree.hs** に対しては **test/MinfreeSpec.hs** という感じです。

```haskell
-- test/MinfreeSpec.hs
module MinfreeSpec (spec) where

import Test.Hspec
import Minfree

spec :: Spec
spec = do
  describe "minfree" $
    it "書籍の実行例" $
      minfree [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15

  describe "minfree'" $
    it "書籍の実行例" $
      minfree' [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15
```

上記の書き方で **minfree** 関数と **minfree'** 関数の入力と出力の振る舞いがテストできるようになりました。

## テストの実行

最後に以下のコマンドでテストを実行します。

```shell-session
$ stack test
Registering PFAD-0.1.0.0...
PFAD-0.1.0.0: test (suite: PFAD-test)

Progress: 1/2
Minfree
  minfree
    書籍の実行例
  minfree'
    書籍の実行例

Finished in 0.0003 seconds
2 examples, 0 failures

PFAD-0.1.0.0: Test suite PFAD-test passed
Completed 2 action(s).
ExitSuccess
```
