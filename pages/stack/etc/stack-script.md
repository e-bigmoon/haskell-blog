---
title: プログラムの配布
---

この方法は厳密には配布とは言えないかもしれませんが、とても小さな規模で、`Qiita` やブログに載せるようなコードの場合は `stack script` で実行してもらいましょう。

似たものに `Script interpreter` 形式もありますが、コードに余分な情報を追加しないといけないという点と、実行時にパッケージの指定をしなければならないという点であまりおすすめしていません。

今回の `src/Minfree.hs` で実際に使い方を見てみましょう。`stack script` 形式では必ず `main` 関数が必要になります (実行するので当たり前と言えば当たり前)。ライブラリであれば `stack ghci` で事足りる場面も多いかもしれません。

`Test.hs`に以下のコードをコピーします。

```haskell:Test.hs
-- main をエクスポートするようにした
module Minfree (main) where

import Data.Array (Array, elems, accumArray, assocs)
import Data.Array.ST (runSTArray, newArray, writeArray)

-- app/Main.hs から持ってきた --
import System.Environment (getArgs)
main :: IO ()
main = do
  [xs] <- getArgs
  print $ minfree $ read xs
---------------------------------

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
  sequence_ [writeArray a x True | x <- xs, x<=n]
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

これを実行してみます。

```shell-session
$ stack script -- Test.hs [0,1,2,3]
When using the script command, you must provide a resolver argument
```

謎のエラーが返ってきました。これは `stack script` コマンドを実行する際には必ず `resolver` を指定する必要があるためです。

```shell-session
$ stack script --resolver=lts-9.17 -- Test.hs [0,1,2,3]
Using resolver: lts-9.17 specified on command line
4
```

このように `resolver` でスナップショットを明示的に指定することで、いつ実行しても同じ結果になります。また、依存しているパッケージなどは自動的に解決してくれるため、とても便利です。
