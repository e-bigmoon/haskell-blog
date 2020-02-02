---
title: ドキュメントの作成
date: 2019/09/14
next: ./haddock-intro.html
---

この章では **haddoc** を用いたドキュメントの作成方法をご紹介します。

**Hackage** アップロードされているパッケージには **haddock** 形式でコメントが残されているものが大半です。そのため、今後ライブラリを作ってみようと考えている方はぜひマスターしましょう。

ドキュメントは `Minfree.hs` に対して追加し、最終的には以下のようなコードが完成します。

```haskell:src/Minfree.hs
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
