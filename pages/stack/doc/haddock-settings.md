---
title: haddockのための設定
---

ここまでである程度 `Haddock` に慣れてきたと思います！

しかし、毎回 `stack haddock --haddock-arguments --odir=haddock` を実行するのは面倒です。規模が小さいうちは処理もそんなに重たく無いのでビルドした際に一緒にドキュメントを生成したいと思うことがあるかもしれません。

そういった時は `stack.yaml` をカスタマイズしてみましょう！

`stack.yaml` のコメントを削除したものがこちらです。(慣れないうちはコメントが非常に有用なのですが、慣れてくると邪魔でしかないです。)

```yaml:stack.yaml
resolver: lts-9.17
packages:
- .
```

ここで `Haddock` に関するデフォルトオプションを明示的に指定してみましょう。(デフォルト値を設定しているだけなので、何も無い場合と全く同じ動作をします)

```yaml:stack.yaml
resolver: lts-9.17
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

試しに `stack haddock` を実行して前と同じものが生成されていることを確認しましょう。

```shell-session
$ stack clean
$ stack haddock --haddock-arguments --odir=haddock
```

では、まずは `--haddock-arguments --odir=haddock` の指定を省略できるように、以下のように設定ファイルを書き換えましょう。

```yaml:stack.yaml
resolver: lts-9.17
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

オプションを指定していませんが、期待通り `haddock` ディレクトリが生成されました！

では次に `stack build` で `haddock` を生成するようにしてみましょう。

```yaml:stack.yaml
resolver: lts-9.17
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

ちゃんと動いていますね。これはどういうことかと言うと実は `stack haddock` は `stack build --haddock` コマンドと同じです。`--haddock` オプションを渡すことで設定ファイルの `haddock: true` と同じ効果があります。

ちなみに `stack test` も同様に `stack build --test` と同じです。

これで `stack build` を行うだけでドキュメントの生成も自動的に行うようにカスタマイズすることができました。

##### open-haddocks オプション

このオプションはビルド完了時に `HTML` ファイルを自動的に開いてくれる設定です。

`true` にしてビルドすると自動的にブラウザが立ち上がり、ドキュメントを開くでしょう。その時のファイルは `all/index.html` を開くため、`base` パッケージのドキュメントなども含まれています。

`--odir` を指定しても、内部に生成された `haddock` を開いてしまうため、基本的にはあまり使いません。

##### 最終的なファイル

コメントを以下のようにつけました。

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

##### haddock
- [Haddock: A Haskell Documentation Tool](https://www.haskell.org/haddock/)
- [Welcome to Haddock’s documentation!](http://haskell-haddock.readthedocs.io/en/latest/index.html)
