---
title: Haddock の設定
published: 2017/12/24
updated: 2019/09/14
prev: ./haddock-comment.html
next: ../test/index.html
---

ここまでである程度 **Haddock** に慣れてきたと思います！この節では **Haddock** に関する設定オプションをいくつか見ていきます。

## stack.yaml の build フィールドを使う

しかし、毎回 `stack haddock --haddock-arguments --odir=haddock` を実行するのは面倒です。規模が小さいうちは **haddock** の生成処理も比較的短時間で終わるため、ビルド時に一緒にドキュメントも生成したいと思うことがあるかもしれません。

そういった時は **stack.yaml** をカスタマイズしてみましょう！

```yaml
# 初期状態の stack.yaml
resolver: lts-14.5
packages:
- .
```

ここで **Haddock** に関するデフォルトオプションを明示的に指定してみましょう。(デフォルト値を設定しているだけなので、何も指定しない場合と全く同じ動作をします)

```yaml:stack.yaml
resolver: lts-14.5
packages:
- .
build:
  haddock: false
  haddock-arguments:
    haddock-args: []
  open-haddocks: false
  haddock-internal: false
  haddock-hyperlink-source: true
```

試しに `stack haddock --haddock-arguments --odir=haddock` を実行して前と同じものが生成されていることを確認しましょう。

```shell-session
$ stack clean
$ stack haddock --haddock-arguments --odir=haddock
```

### --haddock-arguments --odir=haddock の省略

では、まずは `--haddock-arguments --odir=haddock` の指定を省略できるように、以下のように設定ファイルを書き換えましょう。

```yaml
resolver: lts-14.5
packages:
- .
build:
  haddock: false
  haddock-arguments:
    haddock-args:         # [] を削除
    - --odir=haddock      # 追記
  open-haddocks: false
  haddock-internal: false
  haddock-hyperlink-source: true
```

では確認してみましょう。

```shell-session
$ rm -rf ./haddock
$ stack clean
$ stack haddock
```

オプションを指定していませんが、期待通り **haddock** ディレクトリが生成されました！

### stack build で Haddock を自動生成

では次に **stack build** で **haddock** を生成するようにしてみましょう。

```yaml:stack.yaml
resolver: lts-14.5
packages:
- .
build:
  haddock: true              # 変更
  haddock-arguments:
    haddock-args:
    - --odir=haddock
  open-haddocks: false
  haddock-internal: false
  haddock-hyperlink-source: true
```

確かめてみましょう。

```shell-session
$ rm -rf ./haddock
$ stack clean
$ stack build
```

ちゃんと動いていますね。これはどういうことかと言うと実は **stack haddock** は `stack build --haddock` コマンドと同じです。`--haddock` オプションを渡すことで設定ファイルの **haddock: true** と同じ効果があります。

ちなみに **stack test** も同様に `stack build --test` と同じです。**stack install** は `stack build --copy-bins` です。

コマンド | stack build 形式で書いた場合
--------|--------------------------
stack build | `stack build`
stack test | `stack build --test`
stack install | `stack build --copy-bins`
stack haddock | `stack build --haddock`

これで **stack build** を行うだけでドキュメントの生成も自動的に行うようにカスタマイズすることができました。

## 最終的なファイル

コメントを以下のようにつけました。

```haskell
module Minfree (minfree, minfree') where

import Data.Array (Array, elems, accumArray, assocs)
import Data.Array.ST (runSTArray, newArray, writeArray)

-- |
-- 与えられた自然数のリストに含まれない最小の自然数を求める関数
--
-- 自然数は0を含む
--
-- 前提条件1: 与えられたリストには順序がついていない
--
-- 前提条件2: 要素は重複していない
minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

-- | リスト us から vs に含まれる要素をすべて除いた残りの要素のリストを返す
(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

-- |
-- 引数として論理値の配列を取る
-- 論理値のリストに変換して True エントリーからなる最長先頭部分列の長さを返す
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

-- | リストから配列への変換
checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
  where n = length xs

-- | checklist の置き換え
countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0,n) (zip xs (repeat 1))
  where n = maximum xs

sort :: [Int] -> [Int]
sort xs = concat [replicate k x | (x,k) <- assocs (countlist xs)]

-- | Data.Array.ST モジュールを使った checklist
checklist' :: [Int] -> Array Int Bool
checklist' xs = runSTArray $ do
  a <- newArray (0, n) False
  sequence [writeArray a x True | x <- xs, x<=n]
  return a
  where n = length xs

-- | リストを述語 p を満たす要素と満たさない要素のリストに分割する
partition :: (Int -> Bool) -> [Int] -> ([Int], [Int])
partition p xs = (filter p xs, filter (not . p) xs)

-- | 最終的な minfree
minfree' :: [Int] -> Int
minfree' xs = minfrom 0 (length xs, xs)

-- | minfree の一般化
minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs)
  | n == 0 = a
  | m == b - a = minfrom b (n-m, vs)
  | otherwise = minfrom a (m,us)
    where
      (us, vs) = partition (<b) xs
      b = a + 1 + n `div` 2
      m = length us
```
