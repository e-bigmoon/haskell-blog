---
title: 競技プログラミング (AtCoder)
published: 2020/04/08
# updated: 2020/04/08
---

- [AtCoder 公式サイト](https://atcoder.jp/)
- [テストケース](https://www.dropbox.com/sh/arnpe0ef5wds8cv/AAAk_SECQ2Nc6SVGii3rHX6Fa)
- [AtCoder Problems](https://kenkoooo.com/atcoder/)
- [haskell-jp/atcoder-haskell-resources](https://github.com/haskell-jp/atcoder-haskell-resources)

## 入力処理

### 1行に複数の数値

```
5 1 5
```

```hs
main :: IO ()
main = readInts >>= output . solve

readInts :: IO [Int]
readInts = L.unfoldr (C8.readInt . C8.dropWhile C.isSpace) <$> C8.getLine
```

### 1行に数字と文字

文字は **R** か **B** のどちらか。

```
10 B
6 R
```

```hs
main :: IO ()
main = readStrings >>= output . solve . map read'

data Color = R | B
  deriving (Eq, Ord, Read)

read' :: [String] -> (Int, Color)
read' [i, c] = (read i, read c)

readStrings :: IO [String]
readStrings = map C8.unpack . C8.words <$> C8.getLine
```

## 定石

与えられたリストが昇順かどうか判定する関数。

```hs
isAscList :: Ord a => [a] -> Bool
isAscList = and . (zipWith (<) <*> tail)
```

[utility-ht](https://hackage.haskell.org/package/utility-ht) パッケージに [isAscending](https://hackage.haskell.org/package/utility-ht-0.0.15/docs/Data-List-HT.html#v:isAscending) 関数があるので、そちらを使っても良い。

## 参考リソース

- [Haskellで戦う競技プログラミング](https://lab.miz-ar.info/kyopro-haskell/)