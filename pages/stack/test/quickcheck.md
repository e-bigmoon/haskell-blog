---
title: QuickCheck
---

`QuickCheck` は凄く面白いので、`Haskeller` なら使いこなしたいところです。しかしながら、慣れるまでは結構難しいので実例を見ながら使い方を理解していきたいと思います。

###### パッケージのインストール
`HSpec` の時と同様に `package.yaml` の `tests` に `QuickCheck` パッケージを追記します。`quickcheck` では無いのでスペルミスに注意してください。

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
    - hspec
    - QuickCheck # この行を追記
```

`QuickCheck` パッケージは更新が頻繁に行われているのでバージョンごとに書き方が違う場合があります。今回明示的には指定していませんが、`2.9.2` として進めていこうと思います。

###### QuickCheck に慣れよう！
まずは `QuickCheck` が生成するランダムな値について理解を深めたいと思います。

この `sample` 関数を使うことによって、どんな値が生成されるのかデバッグすることができます。型を見る通り `Gen a` の値を適用すれば良さそうに見えますが、ここが少し変わっているので注意してください。

```haskell
$ stack ghci --no-load --package QuickCheck
> import Test.QuickCheck
> :t sample
sample :: Show a => Gen a -> IO ()
```

実際に値をいくつか生成してみます。

```haskell
> sample (arbitrary :: Gen [Int])
[]
[]
[-1,1,-1]
[5,-5]
[-4,-6,-7,-7,1,2,3,8]
[10,3,-2,1]
[11,-8,-11,12,-4,5,-10]
[-3,9,7,6,1]
[1,-12,11,3,8,11,-1,-16]
[-18,-9,-9,-18,9,-15,-3,15,-4,3,-10,8,13,8,15,6,13]
[-9,-15,-4,-2,-7,-1,-2]

> sample (arbitrary :: Gen [Int])
[]
[-2,-2]
[]
[-2]
[8,2,6,-7,7,7,-3,2]
[-6,2,4,-6]
[-5,6,6]
[]
[-6,4,4,1,5,-5,13,-2]
[-1,-17,16]
[-8,17,15,13]

> sample (arbitrary :: [Int])
<interactive>:5:9: error:
    • Couldn't match expected type ‘[Int]’ with actual type ‘Gen a0’
    • In the first argument of ‘sample’, namely ‘(arbitrary :: [Int])’
      In the expression: sample (arbitrary :: [Int])
      In an equation for ‘it’: it = sample (arbitrary :: [Int])

<interactive>:5:9: error:
    • Couldn't match expected type ‘Gen ()’ with actual type ‘[Int]’
    • In the first argument of ‘sample’, namely ‘(arbitrary :: [Int])’
      In the expression: sample (arbitrary :: [Int])
      In an equation for ‘it’: it = sample (arbitrary :: [Int])
```

ここで重要な点は2つです。

- `arbitrary :: Gen [Int]`
- 生成される値は実行のたびにランダムに変化する

面白いので他にも生成してみます。1行で表示させるために `sample'` を利用することにします。

```haskell
> sample' (arbitrary :: Gen Int)
[0,-2,-2,6,6,-2,12,9,-12,-16,5]

> sample' (arbitrary :: Gen [Int])
[[],[0],[2],[-4,1,-2,3,-1],[-6,-3,-3,3,8,-2,-6,8],[9,-1,2,10,1],[-1,1,7,-11,-5,-5,1,-8],[7,-8,2,11,9,-10,2,-5,2],[-6,-13,-13,15,5,8,11,5,3,-9,-14,-8,-11,-13,4,-15],[-10,-11,11,-9,13,-15,14,-14,1,9],[1,-13,-11,0,-16,7,17,-12,-6,-13,-11,-12,9,11]]

> sample' (arbitrary :: Gen Bool)
[True,False,True,True,True,True,False,False,True,False,True]

> sample' (arbitrary :: Gen (Maybe Bool))
[Just True,Just True,Just False,Nothing,Nothing,Just False,Nothing,Just False,Nothing,Just True,Just False]

> sample' (arbitrary :: Gen [a])
<interactive>:20:10: error:
    • No instance for (Arbitrary a1) arising from a use of ‘arbitrary’
      Possible fix:
        add (Arbitrary a1) to the context of
          an expression type signature:
            Gen [a1]
    • In the first argument of ‘sample'’, namely
        ‘(arbitrary :: Gen [a])’
      In the expression: sample' (arbitrary :: Gen [a])
      In an equation for ‘it’: it = sample' (arbitrary :: Gen [a])
```

このように、生成したい値の型を `Gen a` の `a` に指定してあげることでランダムな値を生成できることがわかりました。また、多相型についてはエラーになります。

他にも、`QuickCheck` モジュールではいくつか便利な関数を提供しています。

```haskell
-- 与えられた範囲でランダムな値を生成する
choose :: Random a => (a, a) -> Gen a
> sample' (choose (1,10))
[1,6,10,1,3,1,9,1,1,10,5]


-- 与えられたリストの中からランダムに値を生成する
elements :: [a] -> Gen a
> sample' (elements ["patek", "omega", "seiko"])
["omega","patek","omega","omega","patek","patek","omega","omega","patek","omega","patek"]


-- 与えられたジェネレータのリストの中からランダムに選択し、それを利用してランダムな値を生成する
> let genHighPriceWatch = elements ["patek", "ap", "vc"]
> let genMiddlePriceWatch = elements ["seiko", "omega", "rolex"]
> sample' (oneof [genHighPriceWatch, genMiddlePriceWatch])
["vc","omega","seiko","patek","patek","rolex","rolex","rolex","seiko","omega","seiko"]


-- 出現頻度を指定してランダムな値を生成する (この例では1/100,99/100の出現確率に設定)
frequency :: [(Int, Gen a)] -> Gen a
> let genHighPriceWatchWithFreq = (1, genHighPriceWatch)
> let genMiddlePriceWatchWithFreq = (99, genMiddlePriceWatch)
> sample' (frequency [genHighPriceWatchWithFreq, genMiddlePriceWatchWithFreq])
["seiko","rolex","seiko","omega","seiko","seiko","omega","rolex","omega","omega","omega"]

-- ランダムに生成された値に対して、与えられた条件満たす値のみを生成する
suchThat :: suchThat :: Gen a -> (a -> Bool) -> Gen a
> sample' ((arbitrary :: Gen Int) `suchThat` even)
[-2,0,-4,2,4,6,-10,-10,-8,4,6]

> sample' ((arbitrary :: Gen Int) `suchThat` (>0))
[3,1,5,1,1,10,3,14,11,10,21]

-- ランダムに生成された値のリストを生成する
> sample' (listOf (arbitrary:: Gen Int))
[[],[1,2],[-1,2,-4,2],[5,-1],[6,-3,6,4,0],[-10,8,-10,-5,-2,0,2,-10],[5,5,9,-7,-8,-8,7,9,-6,11],[],[4,14,13,0,13,5],[0,9],[17,-12,12,-3,-7,-13,-1,-1,-19,10,11,16,2,-20,-5,-4,0,12]]

-- 空リストを除く
> sample' (listOf1 (arbitrary:: Gen Int))
[[0],[-1,-1],[4,1,4,0],[2,0],[1,-1,-7,8,-2,1,2,7],[-2,-6,-6,2],[11,-3,-5,4,1,0,-3,9,-10],[2,2,-9,-14,5,13,-13,11,14,12,-9,12,13],[7,10,5,-12,-4],[8,-16],[3,14,2,-17,3,-18,-4,17,16,-2,8,-11,14,20,-1,-10,2]]

-- 長さを指定してランダムな値のリストを生成する
> sample' (vectorOf 3 (arbitrary:: Gen Int))
[[0,0,0],[2,1,1],[3,-1,-2],[5,-2,-5],[2,1,5],[5,-6,4],[2,-12,8],[4,-2,-1],[2,9,-6],[-11,18,-6],[17,-7,-15]]

-- 与えられたリストをシャッフルしたランダムな値のリストを生成する
> sample' (shuffle [1..5])
[[2,5,1,3,4],[3,4,2,1,5],[1,4,5,2,3],[4,1,2,5,3],[5,2,4,1,3],[3,1,4,2,5],[1,3,5,4,2],[1,4,2,3,5],[5,2,3,1,4],[1,4,5,2,3],[4,3,2,1,5]]
```

結構たくさんあるので、基本的なテストであればこれらの関数で十分対応可能です。

```haskell
> sample' $ (vector 3 :: Gen [Int])
[[0,0,0],[2,1,1],[4,-2,2],[3,3,3],[-8,4,5],[5,-10,10],[11,10,4],[-12,9,7],[8,-8,-5],[-15,-3,7],[12,-9,1]]

> sample' (orderedList :: Gen [Int])
[[],[],[1,3],[-4,-2],[-6,-6,6],[-4,-2,0,5,6,8,9,9,10],[-12,-11,-3,1,2,4,5],[-12,-12,-10,-7,9,10,10,11,14],[11],[-17,-16,-11,-8,-3,14],[-19,-19,-15,-14,-12,-1,0,1,4,5,14,18,20]]
```

ここまで具体例をいくつか見てきたので、`QuickCheck` を使う際には `arbitrary` に具体的な型を指定してあげれば良さそうだということがわかってきました。また、`arbitrary` は `Arbitrary` 型クラスのメソッドとなっているため、適切にインスタンスを定義してしまえば、自分で定義した型の値をランダムに生成することも可能です。

##### 実際に QuickCheck のテストを書いてみる
`Peals` の問題は関数に制約をつけていることが多いため、良い練習になりそうです。今回の制約は次の通り

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

##### QuickCheck
- [QuickCheck and Magic of Testing](https://www.fpcomplete.com/blog/2017/01/quickcheck)
- [Test.QuickCheck](https://www.stackage.org/lts-8.19/package/QuickCheck-2.9.2)
- [Property Testing using QuickCheck](https://www.dcc.fc.up.pt/~pbv/aulas/tapf/slides/quickcheck.html#1.0)
