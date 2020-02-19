---
title: HUnit で日本語が文字化けする問題
author: Shinya Yamaguchi
tags: bigmoon, package
---

`HUnit` で日本語を扱おうとすると、以下のように文字化けしますよね。

```hs
expected: "\12495\12473\12465\12523"
 but got: "Haskell"
```

この問題は簡単に回避できるので、その方法を紹介します。やり方は [Human-readable output of unicode characters in expectation results #384](https://github.com/hspec/hspec/issues/384) の方法そのまんまです。

<!--more-->

## 問題が発生している原因

今回使うコードはとてもシンプルです。依存しているパッケージは [HUnit](https://hackage.haskell.org/package/HUnit) です。

```hs
-- Main.hs
import Test.HUnit hiding ((@?=))
import qualified Test.HUnit as HUnit ((@?=))
import Text.Show.Unicode

bad :: IO ()
bad = runTestTT (TestCase $ "Haskell" HUnit.@?= "ハスケル") >> return ()
```

上記のコードを実行すると、以下のような結果になります。

```hs
$ stack repl --package HUnit ./Main.hs
*Main> bad
### Failure:                              
/home/guchi/Desktop/repos/haskell-blog/sample-code/2020/02-19/Main.hs:6
expected: "\12495\12473\12465\12523"
 but got: "Haskell"
Cases: 1  Tried: 1  Errors: 0  Failures: 1
```

この問題は何故発生するかと言うと、`HUnit` 内部で `show` 関数を利用しているためです。

```hs
*Main> putStrLn $ show "ハスケル"
"\12495\12473\12465\12523"
```

上記のような文字化けに対応するには [unicode-show](https://hackage.haskell.org/package/unicode-show) パッケージの **ushow** 関数が便利です。より詳しい紹介は「[日本語をshowしてうまく表示されなかったらunicode-showの紹介（と、pretty-simpleを少し）](https://haskell.jp/blog/posts/2019/unicode-show.html)」を参照してください。

```hs
$ stack repl --package HUnit --package unicode-show ./Main.hs
*Main> import Text.Show.Unicode
*Main Text.Show.Unicode> putStrLn $ ushow "ハスケル"
"ハスケル"
```

ということで、**HUnit** を **ushow** を使うようにしてみましょう。

## 解決策

やり方はとても簡単です。**(@?=)** を以下のように再定義するだけです。

```hs
newtype UString a = UString a
  deriving (Eq)

instance Show a => Show (UString a) where
  show (UString s) = ushow s

(@?=) :: (Eq a, Show a) => a -> a -> HUnit.Assertion
actual @?= expected = UString actual HUnit.@?= UString expected
```

コード全体は以下の通りです。

```hs
import Test.HUnit hiding ((@?=))
import qualified Test.HUnit as HUnit ((@?=))
import Text.Show.Unicode

bad :: IO ()
bad = runTestTT (TestCase $ "Haskell" HUnit.@?= "ハスケル") >> return ()

newtype UString a = UString a
  deriving (Eq)

instance Show a => Show (UString a) where
  show (UString s) = ushow s

(@?=) :: (Eq a, Show a) => a -> a -> Assertion
actual @?= expected = UString actual HUnit.@?= UString expected

good :: IO ()
good = runTestTT (TestCase $ "Haskell" @?= "ハスケル") >> return ()
```

実行してみましょう！

```hs
> good
### Failure:
/home/guchi/Desktop/repos/haskell-blog/sample-code/2020/02-19/Main.hs:15
expected: "ハスケル"
 but got: "Haskell"
Cases: 1  Tried: 1  Errors: 0  Failures: 1
```

こんな感じで他の関数も同様に定義してしまえば良い感じです。

## まとめ

日本語の文字化けで困っている人向け情報でした。

## 参考リソース

- [HUnitで日本語を出力してみる→成功](https://iwamototakashi.hatenadiary.jp/entry/20100722/p1)
- [Human-readable output of unicode characters in expectation results #384](https://github.com/hspec/hspec/issues/384)
- [日本語をshowしてうまく表示されなかったらunicode-showの紹介（と、pretty-simpleを少し）](https://haskell.jp/blog/posts/2019/unicode-show.html)