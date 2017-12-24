---
title: ドキュメントの作成
---

この章では`haddoc`を用いたドキュメントの作成を紹介します。

ドキュメント生成の対象例である`Minfree.hs`を以下に再掲しておきます。

```haskell:src/Minfree.hs
module Minfree (minfree, minfree') where

import Data.Array (Array, elems, accumArray, assocs)
import Data.Array.ST (runSTArray, newArray, writeArray)

minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
  where n = length xs

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0,n) (zip xs (repeat 1))
  where n = maximum xs

sort :: [Int] -> [Int]
sort xs = concat [replicate k x | (x,k) <- assocs (countlist xs)]

checklist' :: [Int] -> Array Int Bool
checklist' xs = runSTArray $ do
  a <- newArray (0, n) False
  sequence [writeArray a x True | x <- xs, x<=n]
  return a
  where n = length xs

partition :: (Int -> Bool) -> [Int] -> ([Int], [Int])
partition p xs = (filter p xs, filter (not . p) xs)

minfree' :: [Int] -> Int
minfree' xs = minfrom 0 (length xs, xs)

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
