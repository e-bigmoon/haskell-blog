---
title: 入出力処理
published: 2020/05/04
# updated: 2020/05/04
---

## 入力

良く使う入力用の関数。

```hs
readInts :: IO [Int]
readInts = L.unfoldr (C8.readInt . C8.dropWhile C.isSpace) <$> C8.getLine

readString :: IO String
readString = C8.unpack <$> C8.getLine

readPair :: IO (Int, Int)
readPair = head . L.unfoldr readT <$> C8.getLine
  where
    readT = runStateT $
      (,) <$> StateT (C8.readInt . C8.dropWhile C.isSpace)
          <*> StateT (C8.readInt . B.unsafeTail)
```

### シンプルな例

[abc165-a]: https://atcoder.jp/contests/abc165/tasks/abc165_a
[abc166-a]: https://atcoder.jp/contests/abc166/tasks/abc166_a

<div style="display: flex; justify-content: space-evenly; align-items: center;">
<div>![[166A][abc166-a]](/images/atcoder/io/166a.png)</div>
<div>
```hs
main = readString >>= output . solve
```
</div>
</div>

---

<div style="display: flex; justify-content: space-evenly; align-items: center;">
<div>![[165A][abc165-a]](/images/atcoder/io/165a.png)</div>
<div>
```hs
main = do
  [k] <- readInts
  readInts >>= output . solve k
```
</div>
</div>

### 複雑な例

[abc166-b]: https://atcoder.jp/contests/abc166/tasks/abc166_b
[abc166-c]: https://atcoder.jp/contests/abc166/tasks/abc166_c

<div style="display: flex; justify-content: space-evenly; align-items: center;">
<div>![[166B][abc166-b]](/images/atcoder/io/166b.png)</div>
<div>
```hs
main = do
  [n,k] <- readInts
  replicateM k (readInts >> readInts) >>= output . solve n
```
</div>
</div>

---

<div style="display: flex; justify-content: space-evenly; align-items: center;">
<div>![[166C][abc166-c]](/images/atcoder/io/166c.png)</div>
<div>
```hs
main = do
  [n,m] <- readInts
  hs <- readInts
  replicateM m readPair >>= output . solve n hs
```
</div>
</div>