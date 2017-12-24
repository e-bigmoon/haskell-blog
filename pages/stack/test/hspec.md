---
title: HSpec
---

まずは `HSpec` を使うために、`package.yaml` の `tests` に `hspec` パッケージを追記します。

```yaml:package.yaml
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

ここで、`test/Spec.hs` の内容を以下のように書き換えます。`hspec-discover` を利用することで、それぞれのソースファイルと一対一に対応した `Spec` ファイルを自動的に読み込んでテストしてくれるようになります。

```:test/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

Spec ファイルの命名規則は、 `src/Minfree.hs` に対しては `test/MinfreeSpec.hs` という感じです。

```:test/MinfreeSpec.hs
module MinfreeSpec (spec) where

import Test.Hspec
import Minfree

spec :: Spec
spec = do
  describe "minfree" $ do
    it "本に載っている例" $ do
      minfree [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15

  describe "minfree'" $ do
    it "本に載っている例" $ do
      minfree' [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15
```

上記の書き方で `minfree` 関数と `minfree'` 関数の入力と出力の振る舞いがテストできるようになりました。

最後に以下のコマンドでテストを実行します。

```shell-session
$ stack test
Registering PFAD-0.1.0.0...
PFAD-0.1.0.0: test (suite: PFAD-test)

Progress: 1/2
Minfree
  minfree
    本に載っている例
  minfree'
    本に載っている例

Finished in 0.0003 seconds
2 examples, 0 failures

PFAD-0.1.0.0: Test suite PFAD-test passed
Completed 2 action(s).
ExitSuccess
```
##### hspec
- [Hspecベストプラクティス](http://fujimura.hatenablog.com/entry/2015/12/15/214332)
- [hspec/hspec-example on github](https://github.com/hspec/hspec-example)
