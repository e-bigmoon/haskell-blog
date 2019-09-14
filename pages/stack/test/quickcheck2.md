---
title: 【実践】ランダムテスト (QuickCheck)
date: 2019/09/14
prev: ./quickcheck.html
# next: ./quickcheck2.html
---

前の章では **QuickCheck** の基本的な関数と使い方を見てきました。この章では実際のコードにテストを追加していきます。

## 実際に QuickCheck のテストを書いてみる

**Peals** の問題は関数に制約をつけていることが多いため、良い練習になりそうです。今回の制約は次の通り

- 自然数
- 重複しない

上記を満たすリストが入力値となります。

###### 素朴に思いつく定義
慣習として `property` のための関数の接頭辞には `prop` をつけます。つけなくても問題は無いです。

```haskell:test/MinfreeSpec.hs
module MinfreeSpec (spec) where

import Test.Hspec
import Minfree
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "minfree" $ do
    it "書籍の例" $ do
      minfree [8, 23, 9, 0, 12, 11, 1, 10, 13, 7, 41, 4, 14, 21, 5, 17, 3, 19, 2, 6] `shouldBe` 15
  describe "minfree'" $ do
    it "書籍の例" $ do
      minfree' [8, 23, 9, 0, 12, 11, 1, 10, 13, 7, 41, 4, 14, 21, 5, 17, 3, 19, 2, 6] `shouldBe` 15
  describe "minfree == minfree'" $ do
    prop "minfree == minfree'" prop_Minfree

prop_Minfree :: [Int] -> Bool
prop_Minfree xs = minfree xs == minfree' xs
```

このコードに対してテストを実行すると次のエラーが表示されます。

```shell-session
Registering PFAD-0.1.0.0...
PFAD-0.1.0.0: test (suite: PFAD-test)


Minfree
  minfree
    書籍の例
  minfree'
    書籍の例
  minfree == minfree'
    minfree == minfree' FAILED [1]

Failures:

  test/MinfreeSpec.hs:17:
  1) Minfree, minfree == minfree', minfree == minfree'
       Falsifiable (after 4 tests and 2 shrinks):
       [-1]

Randomized with seed 1211983148

Finished in 0.0007 seconds
3 examples, 1 failure

PFAD-0.1.0.0: Test suite PFAD-test failed
Completed 2 action(s).
Test suite failure for package PFAD-0.1.0.0
    PFAD-test:  exited with: ExitFailure 1
Logs printed to console
```

エラーメッセージから、どうやらランダムに生成された値に `[-1]` が含まれてたようです。まずはこれを改良してみます。

###### 自然数に限定する
やり方は色々とあると思いますが、今回は `Positive` 型を利用することにします。

```haskell:test/Minfree.hs
prop_Minfree :: [Positive Int] -> Bool
prop_Minfree xs = minfree ns == minfree' ns
  where
    ns = map getPositive xs
```

この結果、また別のエラーが出るようになりました。

```shell-session
Registering PFAD-0.1.0.0...
PFAD-0.1.0.0: test (suite: PFAD-test)


Minfree
  minfree
    書籍の例
  minfree'
    書籍の例
  minfree == minfree'
    minfree == minfree' FAILED [1]

Failures:

  test/MinfreeSpec.hs:17:
  1) Minfree, minfree == minfree', minfree == minfree'
       Falsifiable (after 4 tests and 1 shrink):
       [Positive {getPositive = 1},Positive {getPositive = 1}]

Randomized with seed 398651692

Finished in 0.0028 seconds
3 examples, 1 failure

PFAD-0.1.0.0: Test suite PFAD-test failed
Completed 2 action(s).
Test suite failure for package PFAD-0.1.0.0
    PFAD-test:  exited with: ExitFailure 1
Logs printed to console
```

今回のエラーでは `[1,1]` のような重複した値の場合にテストが失敗しています。これも修正しましょう。

###### 重複をなくす
`(==>)` を使って `minfree` に適用する前に事前条件を設定しておくことにします。

```haskell:test/Minfree.hs
-- import 文を追記
import Data.List (nub)

prop_Minfree :: [Positive Int] -> Property
prop_Minfree xs = preCondition ==> minfree ns == minfree' ns
  where
    ns = map getPositive xs
    preCondition = length (nub xs) == length xs
```

これで `QuciCheck` を記述することができました。


`QuickCheck` を適用した最終的なコードは以下の通りです。

```haskell:test/MinfreeSpec.hs
module MinfreeSpec (spec) where

import Test.Hspec
import Minfree
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.List (nub)

spec :: Spec
spec = do
  describe "minfree" $ do
    it "本に載っている例" $ do
      minfree [8, 23, 9, 0, 12, 11, 1, 10, 13, 7, 41, 4, 14, 21, 5, 17, 3, 19, 2, 6] `shouldBe` 15
  describe "minfree'" $ do
    it "本に載っている例" $ do
      minfree' [8, 23, 9, 0, 12, 11, 1, 10, 13, 7, 41, 4, 14, 21, 5, 17, 3, 19, 2, 6] `shouldBe` 15
  describe "minfree == minfree'" $ do
    prop "minfree == minfree'" prop_Minfree

prop_Minfree :: [Positive Int] -> Property
prop_Minfree xs = preCondition ==> minfree ns == minfree' ns
  where
    ns = map getPositive xs
    preCondition = length (nub xs) == length xs
```

`QuickCheck` は本当に優秀で、自分の書いたコードに安心感をあたえてくれます。しかし、導入までのハードルが少し高いと思うので、日本語による実践的なチュートリアルがもう少し増えて欲しいところです。
