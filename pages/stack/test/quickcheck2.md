---
title: 【実践】ランダムテスト (QuickCheck)
published: 2019/09/14
updated: 2019/09/15
prev: ./quickcheck.html
next: ./doctest.html
---

前の章では **QuickCheck** の基本的な関数と使い方を見てきました。この章では実際のコードにテストを追加していきます。

## 実際に QuickCheck のテストを書いてみる

**Peals** の問題は関数に制約をつけていることが多いため、良い練習になりそうです。

今回の制約は次の通りです。

- 自然数
- 重複しない

上記を満たすリストが入力値となります。

### 素朴に思いつく定義

慣習として **property** のための関数の接頭辞には **prop** をつけます。つけなくても問題は無いです。

```haskell
module MinfreeSpec (spec) where

import Test.Hspec
import Minfree
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "minfree" $
    it "書籍の実行例" $
      minfree [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15

  describe "minfree'" $
    it "書籍の実行例" $
      minfree' [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15

  describe "minfree == minfree'" $
    prop "minfree == minfree'" prop_Minfree

prop_Minfree :: [Int] -> Bool
prop_Minfree xs = minfree xs == minfree' xs
```

このコードに対してテストを実行すると次のようにテストが失敗しました。

```shell-session
$ stack test
...

Minfree
  minfree
    書籍の実行例
  minfree'
    書籍の実行例
  minfree == minfree'
    minfree == minfree' FAILED [1]

Failures:

  test/MinfreeSpec.hs:17:5:
  1) Minfree, minfree == minfree', minfree == minfree'
       Falsifiable (after 3 tests and 2 shrinks):
         [-1]

  To rerun use: --match "/Minfree/minfree == minfree'/minfree == minfree'/"

Randomized with seed 901529074

Finished in 0.0013 seconds
3 examples, 1 failure
...
```

どうやらランダムに生成された値に **[-1]** が含まれてたようです。

まずはこれを改良してみましょう。

### 自然数に限定する

やり方は色々とあると思いますが、今回は **Positive** 型を利用することにします。

```haskell
prop_Minfree :: [Positive Int] -> Bool
prop_Minfree xs = minfree ns == minfree' ns
  where
    ns = map getPositive xs
```

この結果、また別の原因でテストが失敗してしまいました。

```shell-session
$ stack test
...
Failures:

  test/MinfreeSpec.hs:17:5:
  1) Minfree, minfree == minfree', minfree == minfree'
       Falsifiable (after 5 tests and 2 shrinks):
         [Positive {getPositive = 1},Positive {getPositive = 1}]

  To rerun use: --match "/Minfree/minfree == minfree'/minfree == minfree'/"

Randomized with seed 173572505

Finished in 0.0014 seconds
3 examples, 1 failure
...
```

今回は **[1,1]** のような重複した値の場合にテストが失敗しています。これも修正しましょう。

### 重複をなくす

[(==>)](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck.html#v:-61--61--62-) を使って **minfree** に適用する前に事前条件を設定しておくことにします。

```haskell
-- import 文を追記
import Data.List (nub)

prop_Minfree :: [Positive Int] -> Property
prop_Minfree xs = preCondition ==> minfree ns == minfree' ns
  where
    ns = map getPositive xs
    preCondition = length (nub xs) == length xs
```

これで **QuciCheck** を使ったテストを記述することができました。

## 最終的なコード

`QuickCheck` を適用した最終的なコードは以下の通りです。

```haskell
module MinfreeSpec (spec) where

import Test.Hspec
import Minfree
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Data.List (nub)

spec :: Spec
spec = do
  describe "minfree" $
    it "書籍の実行例" $
      minfree [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15

  describe "minfree'" $
    it "書籍の実行例" $
      minfree' [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15

  describe "minfree == minfree'" $
    prop "minfree == minfree'" prop_Minfree

prop_Minfree :: [Positive Int] -> Property
prop_Minfree xs = preCondition ==> minfree ns == minfree' ns
  where
    ns = map getPositive xs
    preCondition = length (nub xs) == length xs
```

**QuickCheck** は本当に優秀で、自分の書いたコードに安心感をあたえてくれます。しかし、導入までのハードルが少し高いと思うので、日本語による実践的なチュートリアルがもう少し増えて欲しいところです。
