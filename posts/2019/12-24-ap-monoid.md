---
title: Ap Monoid
author: Shinya Yamaguchi
tags: bigmoon, monoid, package
---

まずは以下の関数 `f` を考えます。

```haskell
> f = fmap concat . sequence
> e1 = [Just [1,2], Just [3,4]]
> e2 = [Just [1,2], Just [3,4], Nothing]

> f e1
Just [1,2,3,4]

> f e2
Nothing
```

次に、この `f = fmap concat . sequence` をもっと短くカッコよく書けないかな？と考えます。

何となく `mconcat` で置き換えれそうな気がしたんですよ・・・。
だけど、動作が変わってしまうんです・・・。

```haskell
> mconcat e1
Just [1,2,3,4]

> mconcat e2
Just [1,2,3,4]
```

ここで問題です。なぜ `mconcat` にすると結果が異なるのでしょうか？
また、`f = fmap concat . sequence` という定義を畳み込みを使って書き直すことはできるのでしょうか？

このことを考えていたら `Ap` モノイドにたどり着きました。

<!--more-->

## 関数 f は何を行っているのか？

まずは `f = fmap concat . sequence` がどのように動作するか確認してみましょう。

```hs
f e1
  = fmap concat $ sequence [Just [1,2], Just [3,4]]
  = fmap concat $ Just [[1,2], [3,4]]
  = Just $ concat [[1,2], [3,4]]
  = Just [1,2,3,4]

f e2
  = fmap concat $ sequence [Just [1,2], Just [3,4], Nothing]
  = fmap concat Nothing
  = Nothing
```

ここまでは簡単ですね。では `f = mconcat` の場合も確認します。(ここでの定義はデフォルト実装の `mconcat = foldr (<>) mempty` を使います)

```hs
f e1
  = mconcat [Just [1,2], Just [3,4]]
  = foldr (<>) mempty [Just [1,2], Just [3,4]]
  = Just [1,2] <> (Just [3,4] <> mempty)
  = Just [1,2] <> (Just [3,4] <> Nothing)
    -- ココ!!!
  = Just [1,2] <> Just [3,4]
  = Just [1,2,3,4]
```

どうやら `mconcat` に変更すると `Just [3,4] <> Nothing` の計算で結果が変わってくるということがわかります。
つまり、求めている計算は `Monoid` の演算ではなく `Monad (Applicative)` の `Effect` なのです。

```haskell
-- 雰囲気でいえば、こんな感じになれば良いかも？
Just [3,4] <> Nothing -- mconcat で行われる演算
Just [3,4] >> Nothing -- 期待しているのは Maybe モナドの Effect
```

## 畳み込んでみる

何となく方向性は掴めました。

```hs
-- 計算の途中で出てきたこの形を
Just [1,2] <> (Just [3,4] <> Nothing)

-- (とりあえず) こうしたい
Just [1,2] >> (Just [3,4] >> Nothing)
```

形だけ見れば `foldr (>>) mempty` とすれば良さげです。試してみましょう。

```hs
> f = foldr (>>) mempty
> e1 = [Just [1,2], Just [3,4]]
> e2 = [Just [1,2], Just [3,4], Nothing]

> f e1
Nothing

> f e2
Nothing
```

`f e2` は良い感じですが、今度は `f e1` の結果がおかしくなりました・・・。
それはそうですよね。`Maybe` モナドの `Effect` で畳み込んでいるため、どこかで `Nothing` が出てきたら計算全体は `Nothing` になりますよね。

これってつまり、`Maybe` モナドの `Effect` と `リスト` モノイドの演算の両方が必要ってことになりますね・・・。
オリジナルの `f = fmap concat . sequence` の定義も確かにそんな感じです。

## 自分で定義しよう！

もう一度整理しましょう。

```hs
-- 計算対象のリスト
[ m_a1, m_a2, m_a3, me ]

-- まずは Effect を実行したい
m_a1 >> m_a2 >> m_a3 >> me

-- m_a1, m_a2, m_a3, me の結果をそれぞれ a1, a2, a3, e として
-- Monoid の演算を行いたい
a1 <> a2 <> a3 <> e

------------------------------------------------

-- 具体的に、値が全部 Just でラップされていると考えた場合
Just a1 >> Just a2 >> Just a3 >> Just e
-- 期待する結果
a1 <> a2 <> a3 <> e

-- どこか一箇所に Nothing がある (この場合 m_a2 が Nothing) と考えた場合
Just a1 >> Nothing >> Just a3 >> Just e
-- 期待する結果
Nothing
-- 以下のようになってはダメ
a1 <> Nothing <> a3 <> e

------------------------------------------------
-- こんな感じで計算が進むモノイドが欲しい
Just a1 >> Just a2 >> Just a3 >> Just e
  = Just (a1 <> a2) >> Just a3 >> Just e
  = Just (a1 <> a2 <> a3) >> Just e
  = Just (a1 <> a2 <> a3 <> e)

Just a1 >> Nothing >> Just a3 >> Just e
  = Nothing >> Just a3 >> Just e
  = Nothing >> Just e
  = Nothing
```

上記のような挙動を行う `Monoid` を新しく定義してみます！

```hs
newtype Sequence m a = Sequence { getSequence :: m a }
  deriving (Show, Eq)

instance (Monad m, Semigroup a) => Semigroup (Sequence m a) where
  ma <> mb = Sequence $
    do
      a <- getSequence ma
      b <- getSequence mb
      return (a <> b)

instance (Monad m, Monoid a) => Monoid (Sequence m a) where
  mempty = Sequence (return mempty)
```

実際に確かめてみましょう。

```hs
-- (<>) に変更した。
> f = foldr (<>) mempty
> e1 = [Just [1,2], Just [3,4]]
> e2 = [Just [1,2], Just [3,4], Nothing]

> f (map Sequence e1)
Sequence {getSequence = Just [1,2,3,4]}

> f (map Sequence e2)
Sequence {getSequence = Nothing}
```

期待通りに動いていますね。`foldMap` を使うともう少し短くかけます。

```hs
> foldMap Sequence e1
Sequence {getSequence = Just [1,2,3,4]}

> foldMap Sequence e2
Sequence {getSequence = Nothing}
```

## Ap モノイド

ここまでで定義した `Sequence` モノイドは [Data.Monoid](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html) に [Ap](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Ap) という名前で定義されています。

今回は `Monad` で話を進めましたが、実際は `Applicative` でも成り立ちます。
最初の例を **Ap** モノイドで置き換えれば以下のようになります。

```haskell
> import Data.Monoid
> e1 = [Just [1,2], Just [3,4]]
> e2 = [Just [1,2], Just [3,4], Nothing]

> foldMap Ap e1
Ap {getAp = Just [1,2,3,4]}

> foldMap Ap e2
Ap {getAp = Nothing}
```

## まとめ

- `Ap` モノイドの使い道がわかった
- [Haskellerのためのモノイド完全ガイド](https://blog.miz-ar.info/2019/02/monoid-for-haskellers/) でも解説されているので、気になった方はこちらもどうぞ
- `fmap fold . sequenceA` と `getAp . foldMap Ap` を比較すると型制約が若干違う

```hs
> :t fmap fold . sequenceA
fmap fold . sequenceA :: (Monoid a, Traversable t, Applicative f) => t (f a) -> f a

> :t getAp . foldMap Ap
getAp . foldMap Ap    :: (Monoid a, Foldable t, Applicative f) => t (f a) -> f a
```
