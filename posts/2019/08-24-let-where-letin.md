---
title: let, where, let...in の使い分け
author: Shinya Yamaguchi
tags: bigmoon, haskell
---

## はじめに

アルバイトの学生に良く質問される内容の1つに「`let`, `where`, `let...in` はどういう風に使い分ければ良いですか？」というものがあります。ちょっと前に **twitter** でも同じような話題を見かけたので、少しまとめてみました。

また、様々な Haskell 書籍で同様の内容について触れられていますが、明確な指針は無く個人の好みという感じがします。

<!--more-->

## let を使うタイミング (do の中)

```haskell
-- let version
f :: Maybe Int
f = do
  x <- return 1
  let y = x + 1
      z = 2
  return (y+z)
```

- 上記のコードのように `do` の中で束縛された変数 `x` を使って新しい変数 `y` を宣言する時に良く使います
- 他の変数に依存していない `z` はどの書き方でも書けますが、通常は `let` で続けて書くことが多いと思います
- `let` は `do` (またはリスト内包表記) 以外では使えません
- 内部的には `do { let decls; stmts } = let decls in do { stmts }` という変換が行われます。(リスト内包表記は `[ e | let decls, Q ] = let decls in [ e | Q ]` という変換です。)

### where で書いた場合

```haskell
-- where version
f :: Maybe Int
f = do
  x <- return 1
  return ((x+1)+z)
  where
    z = 2
```

- `where` を使って `x` を宣言することはできません。(`do` を脱糖した形を見ると、できない感じが伝わりやすいかもしれません)

```haskell
-- do を使わない形
f = return 1 >>= (\x -> return ((x+1)+z))
  where
    z = 2
```

- 何百行も続く巨大な `do` だと `where` で宣言されている変数までの距離が遠いため、可読性が落ちます。`z` を `where` で書くか `let` で書くかはケースバイケースかもしれません

### let...in で書いた場合

```haskell
-- let...in version
f :: Maybe Int
f = do
  x <- return 1
  let y = x + 1
      z = 0
    in return (y+z)
```

- `let...in` を使うとインデントに気配りする必要があるので面倒です
  - 例えば `let` と `in` の先頭を合わせるとコンパイルエラーになります

```haskell
-- let...in version (compile error)
f :: Maybe Int
f = do
  x <- return 1
  let y = x + 1
      z = 0
  in return (y+z)
```

## where を使うタイミング (ガード)

```haskell
calcBmi :: Double -> Double -> String
calcBmi cm kg
  | bmi <= 18.5 = "痩せてるね"
  | bmi <= 25.0 = "普通だね"
  | bmi <= 30.0 = "ぽっちゃりだね"
  | otherwise = "太っているね"
  where
    bmi = kg / (m^2)
    m = cm / 100

-- *Main> putStrLn $ calcBmi 170 60
-- 普通だね
```

- このように、ガードで共通する変数を宣言する場合に `where` を使うと見通しが良くなります。
- `do` の中ではないため、当然 `let` は使えません

### let...in で書いた場合

```haskell
{-# LANGUAGE MultiWayIf #-}

calcBmi :: Double -> Double -> String
calcBmi cm kg =
  let m   = cm / 100
      bmi = kg / (m^2)
  in if | bmi <= 18.5 -> "痩せてるね"
        | bmi <= 25.0 -> "普通だね"
        | bmi <= 30.0 -> "ぽっちゃりだね"
        | otherwise   -> "太っているね"
```

- もし `let...in` を使って同じように書く場合は、上記のように `MultiWayIf` を使うことになるでしょう。

## let...in を使うタイミング

```haskell
-- Programming in Haskell 2nd Edition より引用
type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))
```

- こんな感じの関数を書くときにはとても便利です
- ただ、このようなケースというのはあまり遭遇しないので、個人的には使わないようにしています。無くてもほぼ困らないです
  - 理由1: レイアウトルールを気にしなければならない点がとても面倒なので
  - 理由2: ポイントフリースタイルで書くのが好きなので

### where で書いた場合

```haskell
-- Programming in Haskell 2nd Edition より引用
type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g = S . f g

f :: (a -> b) -> ST a -> State -> (b, State)
f g st s = (g x, s')
  where
    (x,s') = app st s

-- もしくは
instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S f
    where
      f s = (g x, s')
        where
          (x,s') = app st s
```

- `let...in` で書いたときよりも可読性が落ちてしまったように思います

## where と let...in の違い

`let...in` と `where` の明確な違いは `let...in` が式なのに対して `where` が節だという点です。

```haskell
-- OK
f = if let x = True in x then 1 else 0

-- NG
g = if (x where x = True) then 1 else 0
```

構文上のどこに式が出現できるかという規則については `Language Report` に詳しく記載されています。

## まとめ

このように、`let`, `where`, `let...in` はどちらでも書けることが多いので、適材適所で自分の感性を信じて使いこなすと良いのではないかと思います。

そういえばラムダノートさんから[プログラミングHaskellの第2版](https://www.lambdanote.com/collections/haskell)が発売されましたね。書籍の内容が個人的にとても好きなので、弊社のアルバイトの人には推薦図書として英語版を読んでもらっていました。

これからは日本語で読めるようになったので、これを機に Haskell に入門してみるのも良いかもしれません。

## 参考リソース

- [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/)
- すごいHaskellたのしく学ぼう！
- Programming in Haskell 2nd Edition

## 宣伝

[技術書典7](https://techbookfest.org/event/tbf07)に初参加します。

<img src="/images/2019/08-19/circle.png" alt ="サークルカット" width="400px">

進捗・詳細については [技術書典7特設ページ](/ad/techbookfest7.html) をご確認ください。
