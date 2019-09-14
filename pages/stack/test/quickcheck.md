---
title: 【基礎】ランダムテスト (QuickCheck)
date: 2019/09/14
prev: ./hspec.html
next: ./quickcheck2.html
---

**QuickCheck** は凄く面白いので、**Haskeller** なら使いこなしたいところです。しかしながら、慣れるまでは結構難しいので実例を見ながら使い方を理解していきたいと思います。

## パッケージのインストール

**hspec** の時と同様に **package.yaml** の **tests** に **QuickCheck** パッケージを追記します。**quickcheck** では無いのでスペルミスに注意してください。

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
    - hspec
    - QuickCheck # この行を追記
```

**QuickCheck** パッケージは頻繁に更新されるのでバージョンごとに書き方が違う場合があります。今回明示的には指定していませんが、**lts-14.5** に含まれている [QuickCheck-2.13.2](https://www.stackage.org/lts-14.5/package/QuickCheck-2.13.2) を利用しています。

## QuickCheck に慣れよう

まずは **QuickCheck** が生成するランダムな値について理解を深めたいと思います。

この [sample](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Gen.html#v:sample) 関数を使うことによって、どんな値が生成されるのかデバッグすることができます。型を見る通り [Gen a](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Gen.html#t:Gen) の値を適用すれば良さそうに見えますが、ここが少し変わっているので注意してください。

```haskell
$ stack ghci --no-load --package QuickCheck
λ import Test.QuickCheck
λ :t sample
sample :: Show a => Gen a -> IO ()
```

実際に値をいくつか生成してみます。

```haskell
λ sample (arbitrary :: Gen [Int])
[]
[2]
[-1,-3]
[-5,-6,1,-2,-5,1]
[-8,2,7,2,-6]
[1,6,5,-3,-7,-6]
[-5,9,8,-4,10,-10]
[-12]
[9,15,12,13,-15,-9,-12,8,14,-6,0,3,-4]
[-6,-6,7,-15,-17]
[20,18,3,-13,8,10,6,0]

λ sample (arbitrary :: Gen [Int])
[]
[0]
[]
[-2,-6,3]
[-2,4,-6,8,8,0,7]
[10,-3,0,4,-7,9,8,9]
[4,6,-7,6,-2,-12,-7,-3]
[-6,-9,2,11,11,0]
[-13,9,-9,13,11,-16,1,7,-3]
[-14,11,9,-9,-1,-6,-9]
[-17]

λ sample (arbitrary :: [Int])
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

- **arbitrary :: Gen [Int]**
- 生成される値は実行のたびにランダムに変化する

面白いので他にも生成してみます。1行で表示させるために [sample'](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Gen.html#v:sample-39-) を利用することにします。

```haskell
λ sample' (arbitrary :: Gen Int)
[0,1,0,2,6,-9,-2,-13,2,-9,13]

λ sample' (arbitrary :: Gen [Int])
[[],[2],[-3,-1,-4],[],[-8],[-9,4,-5,-4,5],[3,10,-7,6],[5,5,0,2,0,2],[-2,-7,9,-10,-2,-10,5,-14,3,11,7],[-4],[]]

λ sample' (arbitrary :: Gen Bool)
[True,True,False,False,True,True,False,False,True,False,True]

λ sample' (arbitrary :: Gen (Maybe Bool))
[Nothing,Just False,Just True,Nothing,Just True,Just False,Just False,Nothing,Just False,Just True,Just True]

λ sample' (arbitrary :: Gen [a])
<interactive>:10:10: error:
    • No instance for (Arbitrary a1) arising from a use of ‘arbitrary’
      Possible fix:
        add (Arbitrary a1) to the context of
          an expression type signature:
            forall a1. Gen [a1]
    • In the first argument of ‘sample'’, namely
        ‘(arbitrary :: Gen [a])’
      In the expression: sample' (arbitrary :: Gen [a])
      In an equation for ‘it’: it = sample' (arbitrary :: Gen [a])
```

このように、生成したい値の型を **Gen a** の **a** に指定してあげることでランダムな値を生成できることがわかりました。また、多相型についてはエラーになります。

## 色々な関数を試す

他にも、**Test.QuickCheck** モジュールではいくつか便利な関数を提供しています。

### choose

与えられた範囲でランダムな値を生成します。

```haskell
-- choose :: Random a => (a, a) -> Gen a
λ sample' (choose (1,10))
[7,3,2,5,10,7,8,8,7,6,10]
```

### elements

与えられたリストの中からランダムに値を生成します。

```haskell
-- elements :: [a] -> Gen a
λ sample' (elements ["patek", "omega", "seiko"])
["omega","seiko","patek","omega","patek","seiko","patek","omega","omega","patek","patek"]
```

### oneof

与えられたジェネレータのリストの中からランダムに選択し、それを利用してランダムな値を生成します。

```haskell
-- oneof :: [Gen a] -> Gen a
λ genHighPriceWatch = elements ["patek", "ap", "vc"]
λ genMiddlePriceWatch = elements ["seiko", "omega", "rolex"]
λ sample' (oneof [genHighPriceWatch, genMiddlePriceWatch])
["patek","seiko","ap","rolex","ap","vc","vc","vc","seiko","rolex","patek"]
```

### frequency

出現頻度を指定してランダムな値を生成します。(この例では1/100,99/100の出現確率に設定)

```haskell
-- frequency :: [(Int, Gen a)] -> Gen a
λ genHighPriceWatchWithFreq = (1, genHighPriceWatch)
λ genMiddlePriceWatchWithFreq = (99, genMiddlePriceWatch)
λ sample' (frequency [genHighPriceWatchWithFreq, genMiddlePriceWatchWithFreq])
["omega","omega","seiko","seiko","seiko","seiko","seiko","omega","rolex","seiko","rolex"]
```

### suchThat

ランダムに生成された値に対して、与えられた条件満たす値のみを生成します。

```haskell
-- suchThat :: suchThat :: Gen a -> (a -> Bool) -> Gen a
λ sample' ((arbitrary :: Gen Int) `suchThat` even)
[0,4,4,0,4,10,-4,6,-12,6,2]
λ sample' ((arbitrary :: Gen Int) `suchThat` (>0))
[2,1,5,7,4,4,11,8,4,15,16]
```

### listOf, listOf1

ランダムに生成された値のリストを生成します。

```haskell
-- listOf :: Gen a -> Gen [a]
λ sample' (listOf (arbitrary :: Gen Int))
[[],[-1,-2],[3,-2,-2,1],[2,-1,3,0,5,6],[2,3,1,-2,1,4],[],[-2,1,2,5,-1,-6,2],[-13,9,-12,6,-8],[12,0,9,2,-16,9,6],[17,6,6,18,14,12,-8,15,-11,7,-10,-17,-6],[6,-9,-16,16,19,7,0,1]]
```

空リストを除いた結果が欲しい場合は **listOf1** を利用します。

```haskell
-- listOf1 :: Gen a -> Gen [a]
λ sample' (listOf1 (arbitrary :: Gen Int))
[[0],[-2,-2],[-3,2,-4],[4,5],[2],[-5,-7,4,-4,-1,-3,2,9,0,9],[4],[13],[16,16,11,-11,5,-16,-16,-9,-5,-3],[4,18,14,1,-9,4,-17,10,17,3,-5,8,-5],[4,-1,4,-3,-5,-17,13,6,-6,-7,-9,15,13,0,-8]]
```

### vectorOf

長さを指定してランダムな値のリストを生成します。

```haskell
-- vectorOf :: Int -> Gen a -> Gen [a]
λ sample' (vectorOf 3 (arbitrary :: Gen Int))
[[0,0,0],[1,2,0],[4,4,-3],[-4,6,2],[2,-5,-1],[-1,-5,-4],[-9,11,4],[10,5,3],[-1,-5,-12],[-5,-6,14],[19,-1,-12]]
```

### shuffle

与えられたリストをシャッフルしたランダムな値のリストを生成します。

```haskell
-- shuffle :: [a] -> Gen [a]
λ sample' (shuffle [1..5])
[[1,5,3,2,4],[4,5,1,3,2],[2,1,4,3,5],[3,1,2,5,4],[4,5,1,2,3],[1,3,4,2,5],[3,5,2,1,4],[2,1,4,3,5],[4,3,2,5,1],[1,3,4,2,5],[1,5,2,4,3]]
```

### vector

長さを指定してランダムな値のリストを作成します。

```haskell
-- vector :: Arbitrary a => Int -> Gen [a]
λ sample' $ (vector 3 :: Gen [Int])
[[0,0,0],[-2,1,-1],[-3,-1,4],[1,-1,3],[1,1,4],[10,3,-8],[-8,-4,1],[-11,1,13],[-5,11,10],[-14,-14,-18],[8,0,15]]
```

### orderedList

昇順に並んだランダムな値のリストを生成します。

```haskell
-- orderedList :: (Ord a, Arbitrary a) => Gen [a]
λ sample' (orderedList :: Gen [Int])
[[],[-2,1],[-4,1],[-3,-2,-2,1,6,6],[3],[-10,-6,-1,-1,0,2],[-11,-9,-3],[-12,-11,-10,-6,-2,4,11],[-10,-8,-5,-4,-4,-1,5,5,6,15],[-15,-9,-7,-5,-3,0,5,9,10,14],[-17,-10,-9,-9,1,11]]
```

結構たくさんあるので、基本的なテストであればこれらの関数で十分対応可能です。

ここまで具体例をいくつか見てきたので、**QuickCheck** を使う際には **arbitrary** に具体的な型を指定してあげれば良さそうだということがわかってきました。

また、**arbitrary** は **Arbitrary** 型クラスのメソッドとなっているため、適切にインスタンスを定義してしまえば、自分で定義した型の値をランダムに生成することも可能です。
