---
title: doctest
---

`doctest` は一言で言えば、`Haddock` にテストを埋め込んだものです。

例として、以下のような関数とコメントがあった場合に

```haskell
-- 与えられた価格の消費税を計算する
-- calcSalseTax 100.0 == 8.0
calcSalseTax :: Num a => a -> a
calcSalseTax = (*0.08)
```

消費税が`10%`に変更になったとしたらプログラムは以下のように修正されるでしょう。

```haskell
-- 与えられた価格の消費税を計算する
-- calcSalseTax 100.0 == 8.0
calcSalseTax :: Num a => a -> a
calcSalseTax = (*0.1)
```

この時、ドキュメントも同様に修正されるべきですが、その保証はどこにもありません。この問題に対する有効な解決策が `doctest` です。

###### doctest の準備

まずは `package.yaml` に `doctest` のための記述を追加しましょう。

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
    - QuickCheck
  # ここから下の行を追記
  PFAD-doctest:
    main: test/doctests.hs
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - PFAD
    - doctest
```

このままでは `doctests.hs` が見つからずにエラーになるため、以下のファイルを作成します。このファイルに `doctest` の対象ファイルを記述します。

```haskell:test/doctests.hs
import Test.DocTest
main = doctest ["-isrc", "src/Minfree.hs"]
```

これで準備は完了です。

`doctest` を実行するためには以下のように `stack test` コマンドを実行します。

```shell-session
$ stack clean
$ stack test
```
コマンド実行時に以下のようなエラーが出る場合があります。

```bash
    [16 of 16] Compiling Test.DocTest     ( src/Test/DocTest.hs, .stack-work/dist/x86_64-linux-nopie/Cabal-2.0.1.0/build/Test/DocTest.o )
    /usr/bin/ld.gold: エラー: -ltinfo が見つかりません
    collect2: error: ld returned 1 exit status
    `gcc' failed in phase `Linker'. (Exit code: 1)

```

上記のエラーが出た場合は以下のように必要なパッケージをインストールしてください。

```bash
$ sudo apt install libtinfo-dev
```

##### doctest の書き方

`doctest` は以下のように、非常に直感的に記述することができます。

`doctest` は `>>>` に続く文字列が `ghci` によって処理され、その下の行の結果と等しいかどうかをテストするだけです。

```haskell
-- |
-- >>> add 2 3
-- 5
add x y = x + y
```

それでは `minfree` 関数に `doctest` を記述してみましょう。また、最初なので、間違ったテスト結果を書いてみます。

```haskell:src/Minfree.hs
-- |
-- 与えられた自然数のリストに含まれない最小の自然数を求める関数
--
-- 自然数は0を含む
--
-- 前提条件1: 与えられたリストには順序がついていない
--
-- 前提条件2: 要素は重複していない
--
-- >>> minfree [8, 23, 9, 0, 12, 11, 1, 10, 13, 7, 41, 4, 14, 21, 5, 17, 3, 19, 2, 6]
-- "abcde"
minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)
```

本当にテストが失敗するか確認します。

```shell-session
$ stack test
...
PFAD-0.1.0.0: Test suite PFAD-test passed
Completed 2 action(s).
Test suite failure for package PFAD-0.1.0.0
    PFAD-doctest:  exited with: ExitFailure 1
Logs printed to console
```

確かに失敗していることが確認できました。では、正しいテスト結果を記述してみましょう。

```haskell:src/Minfree.hs
-- |
-- 与えられた自然数のリストに含まれない最小の自然数を求める関数
--
-- 自然数は0を含む
--
-- 前提条件1: 与えられたリストには順序がついていない
--
-- 前提条件2: 要素は重複していない
--
-- >>> minfree [8, 23, 9, 0, 12, 11, 1, 10, 13, 7, 41, 4, 14, 21, 5, 17, 3, 19, 2, 6]
-- 15
minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)
```

もう一度テストしてみます。

```shell-session
$ stack test
...

$ stack haddock
```

今度はちゃんとテストをパスし、以下のようなドキュメントが生成されると思います。

![スクリーンショット 2017-12-11 18.27.52.png](/images/doctest01.png)

また `QuichCheck` を使った書き方もできます。その場合は `>>>` を `prop>` にするだけです。

```haskell:src/Minfree.hs
-- Haddock に表示させたいのでエクスポートしています
module Minfree (minfree, minfree', (\\)) where

...

-- | リスト us から vs に含まれる要素をすべて除いた残りの要素のリストを返す
--
-- prop> (as ++ bs) \\ cs == (as \\ cs) ++ (bs \\ bs)
(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us
```

この性質は満たされないためテストに失敗します。

```shell-session
$ stack test
### Failure in src/Minfree.hs:21: expression `(as ++ bs) \\ cs == (as \\ cs) ++ (bs \\ bs)'
*** Failed! Falsifiable (after 3 tests and 3 shrinks):
[]
[0]
[]
```

正しく書き換えた場合はテストに通ります。ついでに、残りのプロパティテストも追加しておきます。

```haskell:src/Minfree.hs
-- Haddock に表示させたいのでエクスポートしています
module Minfree (minfree, minfree', (\\)) where

...

-- | リスト us から vs に含まれる要素をすべて除いた残りの要素のリストを返す
--
-- prop> (as ++ bs) \\ cs == (as \\ cs) ++ (bs \\ cs)
--
-- prop> as \\ (bs ++ cs) == (as \\ bs) \\ cs
--
-- prop> (as \\ bs) \\ cs == (as \\ cs) \\ bs
(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us
```

実行するとちゃんとテストをパスしてドキュメントを生成していると思います。

```shell-session
$ stack test
```

![スクリーンショット 2017-12-11 18.57.01.png](/images/doctest02.png)

なんかかっこいい感じのドキュメントになってきました！

こんな感じでドキュメントも手軽にテストできるので、ぜひアプリケーションを開発する際に利用してください。

##### doctest
- [sol/doctest](https://github.com/sol/doctest)