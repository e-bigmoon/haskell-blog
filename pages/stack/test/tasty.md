---
title: テストフレームワーク (tasty)
date: 2019/09/15
prev: ./doctest.html
next: ../bench/index.html
---

この章では **hspec** とは別のテストフレームワークとして **tasty** の使い方を見ていきましょう。実際のプロジェクトでは、どちらかを採用するという場合が多いと思います。

**hspec** のテストとかぶってしまわないように、新しく **test-tasty** ディレクトリを作成します。

```shell
$ mkdir test-tasty
```

## tasty-discover について

**hspec-discover** と同じような名前の [tasty-discover](https://hackage.haskell.org/package/tasty-discover) というツールがあります。

**tasty-discover** を使えば、ルールに沿って関数を定義するだけえ自動的にテストを見つけてくれます。

ルールは以下のように、関数に特定の接頭辞を付けることで **stack test** を実行した際、それぞれの関数がどのテストを実行するか判断します。

関数の接頭辞 | テスト
-----------|----------
`prop_` | QuickCheck
`scprop_` | SmallCheck
`hprop_` | Hedgehog
`unit_` | HUnit
`spec_` | Hspec
`test_` | Tasty

まずはインストールしましょう。

### tasty-discover のインストール

```shell
$ stack install tasty-discover --resolver lts
$ tasty-discover
Usage: tasty-discover src _ dst [OPTION...]
```

次に **tasty-discover** を利用するために **test-tasty/Tasty.hs** を作ります。

```haskell
-- test-tasty/Tasty.hs
{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
```

今までと同様に **test** セクションに **PFAD-tasty** を追加します。

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
    - QuickCheck
  PFAD-doctest:
    main: test/doctests.hs
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
    - doctest
  # ここから下の行を追記
  PFAD-tasty:
    main: Tasty.hs
    source-dirs: test-tasty
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
    - tasty
```

まだテストケースは何も無い状態ですが、一旦ビルドが通るかどうかテストしてみましょう。

```shell
$ stack test PFAD:test:PFAD-tasty
...

PFAD> test (suite: PFAD-tasty)

Progress 1/2: PFAD
All 0 tests passed (0.00s)

...
```

良さそうですね。

## テストファイルの作成

**hspec-discover** の場合はファイルの命名規則がありましたが、**tasty-discover** にそういう規則はありません。

そのため、今回は **test-tasty/Test/Minfree.hs** にテストを書いていこうと思います。

```shell
$ mkdir test-tasty/Test
```

```haskell
module Test.Minfree () where

import Minfree

import Test.Tasty
```

まだ何もテストが無い状態なので当然何も起きません。

```shell
$ stack test PFAD:test:PFAD-tasty
...

PFAD> test (suite: PFAD-tasty)

Progress 1/2: PFAD
All 0 tests passed (0.00s)

...
```

### tasty-hunit

**hspec** と同じテストを追加してみましょう。

**tasty** で単体テストを行うためには [tasty-hunit](https://hackage.haskell.org/package/tasty-hunit) パッケージを使います。

```yaml
# package.yaml
tests:
...

  PFAD-tasty:
    main: Tasty.hs
    source-dirs: test-tasty
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
    - tasty
    - tasty-hunit   # この行を追記
```

テストを追加します。単体テストの場合は関数の接頭辞に **unit_** をつけると自動的にテストが実行されます。

```haskell
-- test-tasty/Test/Minfree.hs
module Test.Minfree where

import Minfree

import Test.Tasty
import Test.Tasty.HUnit

unit_case1 :: IO ()
unit_case1 = minfree [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] @?= 15

unit_case2 :: IO ()
unit_case2 = minfree' [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] @?= 15
```

```shell
$ stack test PFAD:test:PFAD-tasty
...

Progress 1/2: PFADtest-tasty/Tasty.hs
  case1: OK
  case2: OK

All 2 tests passed (0.00s)
```

上記の出力結果からわかるように **unit_** 以降の関数名 (**test1**, **test2**) がテストケースの名前として表示されます。

### tasty-quickcheck

**QuickCheck** も同様に **tasty** で書く事ができます。その場合は [tasty-quickcheck](https://hackage.haskell.org/package/tasty-quickcheck) パッケージを追加します。

```yaml
tests:
...

  PFAD-tasty:
    main: Tasty.hs
    source-dirs: test-tasty
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
    - tasty
    - tasty-hunit
    - tasty-quickcheck    # この行を追記
```

テストを追加します。**QiuckCheck** の場合は関数の接頭辞に **prop_** をつけると自動的にテストが実行されます。

```haskell
module Test.Minfree where

import Minfree

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.List (nub)

unit_case1 :: IO ()
unit_case1 = minfree [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] @?= 15

unit_case2 :: IO ()
unit_case2 = minfree' [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] @?= 15

prop_Minfree :: [Positive Int] -> Property
prop_Minfree xs = preCondition ==> minfree ns == minfree' ns
  where
    ns = map getPositive xs
    preCondition = length (nub xs) == length xs
```

```shell
$ stack test PFAD:test:PFAD-tasty
...

Progress 1/2: PFADtest-tasty/Tasty.hs
  case1:   OK
  case2:   OK
  Minfree: OK (0.04s)
    +++ OK, passed 100 tests; 349 discarded.

All 3 tests passed (0.04s)
```

### tasty-hspec

面白いことに **hspec** のテストも **tasty** で書く事ができます。その場合は [tasty-hspec](https://hackage.haskell.org/package/tasty-hspec) パッケージを利用します。

```yaml
tests:
...

  PFAD-tasty:
    main: Tasty.hs
    source-dirs: test-tasty
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
    - tasty
    - tasty-hunit
    - tasty-quickcheck
    - tasty-hspec         # この行を追記
    - hspec               # この行を追記
```

テストを追加します。**hspec** の場合は関数の接頭辞に **spec_** をつけると自動的にテストが実行されます。

```haskell
module Test.Minfree where

import Minfree

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Hspec
import Test.Hspec.QuickCheck (prop)

import Data.List (nub)

unit_case1 :: IO ()
unit_case1 = minfree [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] @?= 15

unit_case2 :: IO ()
unit_case2 = minfree' [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] @?= 15

prop_Minfree :: [Positive Int] -> Property
prop_Minfree xs = preCondition ==> minfree ns == minfree' ns
  where
    ns = map getPositive xs
    preCondition = length (nub xs) == length xs

spec_hspec :: Spec
spec_hspec = do
  describe "minfree" $
    it "書籍の実行例" $
      minfree [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15

  describe "minfree'" $
    it "書籍の実行例" $
      minfree' [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15

  describe "minfree == minfree'" $
    prop "minfree == minfree'" prop_Minfree
```

```shell
$ stack test PFAD:test:PFAD-tasty
...

test-tasty/Tasty.hs
  case1:                   OK
  case2:                   OK
  Minfree:                 OK (0.05s)
    +++ OK, passed 100 tests; 387 discarded.
  hspec
    minfree
      書籍の実行例:        OK
    minfree'
      書籍の実行例:        OK
    minfree == minfree'
      minfree == minfree': OK (0.05s)

All 6 tests passed (0.07s)
```

### tasty

**tasty** の形式も当然書く事ができます。

接頭辞は **test_** です。

```haskell
module Test.Minfree where

import Minfree

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Hspec
import Test.Hspec.QuickCheck (prop)

import Data.List (nub)

unit_case1 :: IO ()
unit_case1 = minfree [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] @?= 15

unit_case2 :: IO ()
unit_case2 = minfree' [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] @?= 15

prop_Minfree :: [Positive Int] -> Property
prop_Minfree xs = preCondition ==> minfree ns == minfree' ns
  where
    ns = map getPositive xs
    preCondition = length (nub xs) == length xs

spec_hspec :: Spec
spec_hspec = do
  describe "minfree" $
    it "書籍の実行例" $
      minfree [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15

  describe "minfree'" $
    it "書籍の実行例" $
      minfree' [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6] `shouldBe` 15

  describe "minfree == minfree'" $
    prop "minfree == minfree'" prop_Minfree

test_tasty :: TestTree
test_tasty = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      "abc" `compare` "ab" @?= GT
  , testCase "List comparison (same length)" $
      "abc" `compare` "abb" @?= GT
  ]
```

```shell
$ stack test PFAD:test:PFAD-tasty
...
Progress 1/2: PFADtest-tasty/Tasty.hs
  case1:                                OK
  case2:                                OK
  Minfree:                              OK (0.04s)
    +++ OK, passed 100 tests; 453 discarded.
  hspec
    minfree
      書籍の実行例:                     OK
    minfree'
      書籍の実行例:                     OK
    minfree == minfree'
      minfree == minfree':              OK (0.04s)
  Unit tests
    List comparison (different length): OK
    List comparison (same length):      OK

All 8 tests passed (0.04s)
```

### tasty-expected-failure

時には失敗系のテストを書く事もありますよね。そういう場合は [tasty-expected-failure](https://hackage.haskell.org/package/tasty-expected-failure) パッケージを使います。

```yaml
tests:
...

  PFAD-tasty:
    main: Tasty.hs
    source-dirs: test-tasty
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
    - tasty
    - tasty-hunit
    - tasty-quickcheck
    - tasty-hspec
    - hspec
    - tasty-expected-failure  # この行を追加
```

[expectFail](https://hackage.haskell.org/package/tasty-expected-failure-0.11.1.1/docs/Test-Tasty-ExpectedFailure.html#v:expectFail) 関数を利用します。**expectFail** は **TestTree** を返すため **test_** の接頭辞を使えば良いです。

```haskell
...
import Test.Tasty.ExpectedFailure

...

test_failure :: TestTree
test_failure = expectFail $ testGroup "Unit tests"
  [ testCase "different length" $
      length "abc" @?= 0
  ]
```

```shell
$ stack test PFAD:test:PFAD-tasty
Progress 1/2: PFADtest-tasty/Tasty.hs
...

  Unit tests
    different length:                   FAIL (expected)
      test-tasty/Test/Minfree.hs:50:
      expected: 0
       but got: 3(expected failure)

All 9 tests passed (0.03s)
```

### tasty-golden

ゴールデンテストは回帰テストみたいな感じです。**CSV** や **json** などの比較に良く使います。[tasty-golden](https://hackage.haskell.org/package/tasty-golden) パッケージを利用します。

```yaml
tests:
...

  PFAD-tasty:
    main: Tasty.hs
    source-dirs: test-tasty
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
    - tasty
    - tasty-hunit
    - tasty-quickcheck
    - tasty-hspec
    - hspec
    - tasty-expected-failure
    - tasty-golden            # この行を追加
```

まずは [goldenVsFile](https://hackage.haskell.org/package/tasty-golden-2.3.2/docs/Test-Tasty-Golden.html#v:goldenVsFile) を使ってみます。この関数には、期待する結果のファイル (今回は **golden**) と テストで出力された結果を含むファイル (今回は **output**) を指定し、さらに実際の処理を記述します。

```haskell
...
import Test.Tasty.Golden

...

test_goldenFile :: TestTree
test_goldenFile = goldenVsFile "goldenVsFile" "./test-tasty/golden" "./test-tasty/output" $
  writeFile "./test-tasty/output" "aaa"
```

ゴールデンファイルが存在しない場合は自動的に出力結果と同じものが生成されます。

```shell
$ stack test PFAD:test:PFAD-tasty
Progress 1/2: PFADtest-tasty/Tasty.hs
...
  goldenVsFile:                         OK (0.05s)
    Golden file did not exist; created

All 10 tests passed (0.28s)
```

現在のゴールデンファイルは以下のようになっています。

```text
# test-tasty/goldenファイルの中身
aaa
```

このファイルを適当な値で変更してみましょう。

```text
# test-tasty/goldenファイルの中身(変更後)
bbb
```

```shell
$ stack test PFAD:test:PFAD-tasty
...

  goldenVsFile:                         FAIL
    Files './test-tasty/golden' and './test-tasty/output' differ

1 out of 10 tests failed (0.14s)
```

テストの結果は失敗です。内容が異なっているということがわかりました。

また、ファイルとの比較ではなく、文字列と比較する [goldenVsString](https://hackage.haskell.org/package/tasty-golden-2.3.2/docs/Test-Tasty-Golden.html#v:goldenVsString) という関数もあります。

#### diff の表示

さきほどの関数では表示結果が異なっていることしかわかりませんでした。実際に差分を表示させたい場合は [goldenVsFileDiff](https://hackage.haskell.org/package/tasty-golden-2.3.2/docs/Test-Tasty-Golden.html#v:goldenVsFileDiff) 関数を使います。

```haskell
...
import Test.Tasty.Golden

...

test_goldenFileDiff :: TestTree
test_goldenFileDiff = goldenVsFileDiff "goldenVsFileDiff" (\ref new -> ["diff", "-u", ref, new]) "./test-tasty/golden" "./test-tasty/outputDiff" $
  writeFile "./test-tasty/outputDiff" "aaa"
```

この関数は差分を表示させるための引数を取ります。基本的な使い方としては `(\ref new -> ["diff", "-u", ref, new])` で良いでしょう。

```shell
$ stack test PFAD:test:PFAD-tasty
...

  goldenVsFileDiff:                     FAIL
    --- ./test-tasty/golden     2019-07-28 18:33:13.000000000 +0900
    +++ ./test-tasty/outputDiff 2019-07-28 18:40:09.000000000 +0900
    @@ -1 +1 @@
    -bbb
    \ No newline at end of file
    +aaa
    \ No newline at end of file

2 out of 11 tests failed (0.10s)
```

こんな感じで差分が表示されるため、デバッグがしやすくなります。この関数にも文字列バージョンの [goldenVsStringDiff](https://hackage.haskell.org/package/tasty-golden-2.3.2/docs/Test-Tasty-Golden.html#v:goldenVsStringDiff) があります。
