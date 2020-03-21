---
title: extensible チュートリアル 1
published: 2020/02/19
# updated: 2020/02/22
---

## 目次

- [モチベーション](#モチベーション)
- [道のり1. タプル](#道のり1.-タプル)
- [道のり2. mapTuple3 を自分で定義してみる](#道のり2.-maptuple3-を自分で定義してみる)
- [道のり3. RankNTypes](#道のり3.-rankntypes)
- [道のり4. 制約 (Constraint) を追加する](#道のり4.-制約-constraint-を追加する)
- [道のり5. 制約を抽象化する](#道のり5.-制約を抽象化する)
- [道のり6. 型レベルリスト (任意長引数への拡張)](#道のり6.-型レベルリスト-任意長引数への拡張)
- [道のり7. extensible のすすめ](#道のり7.-extensible-のすすめ)

## モチベーション

こんな関数を定義したことありませんか？

```hs
showArgs :: Int -> Bool -> Maybe Char -> [String]
showArgs a b c = [ show a, show b, show c ]
```

この `showArgs` 関数は正しく動作します。

```hs
> showArgs 3 True (Just 'a')
["3","True","Just 'a'"]
```

さて、今回の話はこの `showArgs` をより一般化したい。

つまり、

- 引数の型が全て異なっても動いて欲しい
- `show` をリストの中に埋め込みたく無い
- 1つの関数定義で任意長の引数を扱えるようにしたい

ということです。

この例を通して `RankNTypes`, `型レベルリスト`, `extensible package` への道を作りたいと思います。

## 道のり1. タプル

まず、リストは同じ型の値しかリストに入れることができないので、複数の異なる型の値を1つの値として扱うことができるタプルを利用してみましょう。

```hs
showArgsTuple :: (Int, Bool, Maybe Char) -> (String, String, String)
showArgsTuple (a, b, c) = (show a, show b, show c)
```

```hs
> showArgsTuple (3, True, Just 'a')
("3","True","Just 'a'")
```

ここまでは順調です。あとはタプルの各要素に `show` 関数を適用する何かを定義するだけです。

すぐに思いつくのは `Functor` 型クラスの `fmap` ですが、これは上手くいきません。インスタンス定義が無いと言われてエラーになります。

```hs
> :set -XFlexibleContexts
> fmap show (1,True,Just 'a')
No instance for (Functor ((,,) Integer Bool))
```

インスタンス定義が無いなら自分で定義すれば良いですね。

```haskell
instance Functor ((,,) a b) where
  -- fmap :: (c -> d) -> (a, b, c) -> (a, b, d)
  fmap f (a, b, c) = (a, b, f c)
```

しかし、定義してみるとわかりますが、最後の要素にしか `f` は適用できないのです・・・。

`fmap :: (a -> b) -> f a -> f b` なので当然といえば当然です・・・。

```hs
> fmap show (1,True,Just 'a')
(1,True,"Just 'a'")
```

今欲しい結果は `(a, b, f c)` ではなく `(f a, f b, f c)` なので、これでは失敗です！

`Functor` の力を借りることができないので、自分で新しく関数を定義することにしましょう。

## 道のり2. mapTuple3 を自分で定義してみる

まずは1引数の関数に限定して考えることにします。

```hs
badMapTuple3 :: (a -> r) -> (a, b, c) -> (r, r, r)
badMapTuple3 f (a, b, c) = (f a, f b, f c)
```

素朴に考えたらこんな感じの定義になるのではないでしょうか。

しかし、これはコンパイルに失敗します・・・。

```hs
Couldn't match expected type ‘a’ with actual type ‘b’
Couldn't match expected type ‘a’ with actual type ‘c’
```

なぜコンパイルに失敗するかと言うと型変数 `a` は以下のように束縛されているからです。

```hs
-- 型 a のみ、明示的に `forall` を書いた (b, cは省略)
badMapTuple3 :: forall a . (a -> r) -> (a, b, c) -> (r, r, r)
```

もし、型変数 `a` が `Int` の場合は以下のようになります。

```hs
badMapTuple3 :: (Int -> r) -> (Int, b, c) -> (r, r, r)
```

これで `f a` は適用できるけども `f b`, `f c` が不可能だと言うことがわかりました。
なぜなら、型変数 `b`, `c` は `Int` 以外の型になる可能性があるためです。

## 道のり3. RankNTypes

`badMapTuple3` の問題は `forall` の位置にあります。この問題を解決するためには、どんな型の引数でも動作するような総称的な関数を引数に取れば良いのです。(つまり、これは型クラスのメソッドです)

先に正しく動作する `goodMapTuple3` を定義して比較してみましょう。定義するためには `RankNTypes` が必要です。

```hs
badMapTuple3  ::  forall a. (a -> r) -> (a, b, c) -> (r, r, r)
badMapTuple3  f (a, b, c) = (f a, f b, f c)

goodMapTuple3 :: (forall a.  a -> r) -> (a, b, c) -> (r, r, r)
goodMapTuple3 f (a, b, c) = (f a, f b, f c)
```

実装は全く同じですが、`goodMapTuple3` では `forall` が型の第一引数の関数の中にあります。(一般的に `badMapTuple3` のような関数をランク1の関数, `goodMapTuple3` のような関数をランク2の関数と呼びます)

つまり、関数 `f` はどんな型の値が来ても `r` 型の値を返す関数ということです。

```hs
f :: Int -> r
f :: Char -> r
f :: Maybe Int -> r
f :: [a] -> r
...
```

具体例をいくつか考えてみましょう。

```hs
-- const 1 はどんな値が来ても常に1を返す関数
> goodMapTuple3 (const 1) (1, True, Just 'a')
(1,1,1)
```

いくつか考えようと思いましたが、意外と思いつきません。つまり、適用範囲が広い分、言えること (できること) が少ないんです。`forall` は `すべての` と言う意味なので、すべての型の値について正しく動作する必要があるのです。

## 道のり4. 制約 (Constraint) を追加する

`forall` は扱える範囲は広いけど、具体的に何か処理を行おうとすると少し不便です。そのため、`forall` に制約を加えます。

ここでいう制約と言うのは、新しいものではなく、Haskell で必ず学習する**型クラス制約**のことです。具体的に言えば `Eq`, `Show`, `Num`, `Monad` などたくさんあります！

ようするに、`forall` (すべて) では対象が広すぎ、具体的な型では対象が狭すぎるため、型クラス制約を満たす型という感じで限定するんです。雰囲気こんな感じです。

![constraint.png](/images/libraries/extensible/constraint.png)

今回は `Show` について考えていたので、`Show` 型クラス制約を用いて `goodMapTuple3` を書き直してみましょう。

```hs
mapShowTuple3 ::
  (Show a, Show b, Show c) =>
  (forall x. Show x => x -> r) -> (a, b, c) -> (r, r, r)
mapShowTuple3 f (a,b,c) = (f a, f b, f c)
```

一番重要なのは `forall x. Show x =>` の部分です、ここで関数が動作する型の範囲を限定しています。

実行してみましょう。

```hs
> mapShowTuple3 show (1, True, Just 'a')
("1","True","Just 'a'")
```

素晴らしいですね！これで

- 引数の型が全て異なっても動いて欲しい
- `show` をリストの中に埋め込みたく無い
- 1つの関数定義で任意長の引数を扱えるようにしたい

の1つ目と2つ目を達成することができました！

鋭い方はお気づきかもしれませんが、`mapShowTuple3` は関数の型に `Show` が埋め込まれているため任意の制約については扱うことができません。

このままでは、実質的には `show` 関数が埋め込まれていることと同じです。次のステップでこの制限を無くしてみましょう。

## 道のり5. 制約を抽象化する

変数名 `a`,`b`,`c` を `x1`,`x2`,`x3` に変更した `mapShowTuple3` を使って話を進めます。

```hs
mapShowTuple3 ::
  (Show x1, Show x2, Show x3) =>
  (forall x. Show x => x -> r) -> (x1, x2, x3) -> (r, r, r)
mapShowTuple3 f (x1, x2, x3) = (f x1, f x2, f x3)
```

上記の定義を眺めていると、`Show` の部分を何らかの変数 `c` で置き換え、呼び出し側で `Show` を渡すことができれば上手くいきそうな気がしますね。

つまり、以下のように抽象化できそうです。

```hs
mapShowTuple3 ::
  (c x1, c x2, c x3) =>
  (forall x. c x => x -> r) -> (x1, x2, x3) -> (r, r, r)
mapShowTuple3 f (x1, x2, x3) = (f x1, f x2, f x3)
```

次に気になるのは、型変数 `c` のカインドです。`Show` の場合どうなっているのか確認してみましょう。

```hs
> :set -XNoStarIsType 
> :k Show
Show :: Type -> Constraint
```

`Type` カインドを受け取って `Constraint` カインドを返すようなカインドになっていますね。型変数 `c` のカインドも同じようにしてみましょう。

このコードを動かすためには `KindSignatures`, `ConstraintKinds` 言語拡張が必要になります。

```hs
mapTuple3 ::
  forall (c :: Type -> Constraint) x1 x2 x3 r.
  (c x1, c x2, c x3) =>
  (forall x. c x => x -> r) -> (x1, x2, x3) -> (r, r, r)
mapTuple3 f (a, b, c) = (f a, f b, f c)
```

とても良さそうですが、まだコンパイルは通りません。

なぜなら、制約 `c` が指定されていないため曖昧になるからです。ちゃんと `c` を指定できるようにしましょう。

```hs
mapTuple3 ::
  forall (c :: Type -> Constraint) x1 x2 x3 r.
  (c x1, c x2, c x3) =>
  c -> (forall x. c x => x -> r) -> (x1, x2, x3) -> (r, r, r)
mapTuple3 _ f (a, b, c) = (f a, f b, f c)
```

これでもまだコンパイルエラーになります。

```hs
Expecting one more argument to ‘c’
Expected a type, but ‘c’ has kind ‘Type -> Constraint’
```

これは `->` のカインドが `Type -> Type -> Type` なので、カインドが合わなくてコンパイルエラーになっています。

```hs
> :k (->)
(->) :: Type -> Type -> Type
```

そのため、ここで `Proxy` 型を使って `c` ではなく `Proxy c` として制約を受け取ります。

```hs
mapTuple3 ::
  forall (c :: Type -> Constraint) x1 x2 x3 r.
  (c x1, c x2, c x3) =>
  Proxy c -> (forall x. c x => x -> r) -> (x1, x2, x3) -> (r, r, r)
mapTuple3 _ f (a, b, c) = (f a, f b, f c)
```

これでコンパイルが通るようになりました。ここで気にして欲しい点としては `Proxy c` 型の値には興味が無いというところです。制約を伝えるためだけに導入した引数なので、値 (`Proxy` 型の値は `Proxy` なので何も情報を持ちません) は `_` で捨てています。


実際に使ってみましょう。

```hs
> mapTuple3 (Proxy @Show) show (1,True,Just 'a')
("1","True","Just 'a'")
```

他の例も試してみましょう。

```hs
> import Data.Typeable
> mapTuple3 (Proxy @Show) show (1,True,Just 'a')
(Integer,Bool,Maybe Char)

> import Data.Aeson
> mapTuple3 (Proxy @ToJSON) encode (1,True,Just 'a')
("1","true","\"a\"")
```

これで期待通りの動作になりました！！！

ここまで作ってきた関数をまとめてみましょう。

```hs
-- オリジナル
showArgs :: Int -> Bool -> Maybe Char -> [String]
showArgs a b c = [ show a, show b, show c ]

-- タプルバージョン
showArgsTuple :: (Int, Bool, Maybe Char) -> (String, String, String)
showArgsTuple (a, b, c) = (show a, show b, show c)

-- mapTuple3 (コンパイルが通らないバージョン)
badMapTuple3 :: (a -> r) -> (a, b, c) -> (r, r, r)
badMapTuple3 f (a, b, c) = (f a, f b, f c)

-- mapTuple3 (forall バージョン)
goodMapTuple3 :: (forall a. a -> r) -> (a, b, c) -> (r, r, r)
goodMapTuple3 f (a, b, c) = (f a, f b, f c)

-- mapTuple3 (Show バージョン)
mapShowTuple3 ::
  (Show a, Show b, Show c) =>
  (forall x. Show x => x -> r) -> (a, b, c) -> (r, r, r)
mapShowTuple3 f (a, b, c) = (f a, f b, f c)

-- mapTuple3 (任意の制約で動くバージョン)
mapTuple3 ::
  forall (c :: Type -> Constraint) x1 x2 x3 r.
  (c x1, c x2, c x3) =>
  Proxy c -> (forall x. c x => x -> r) -> (x1, x2, x3) -> (r, r, r)
mapTuple3 _ f (a, b, c) = (f a, f b, f c)
```

あとはこの関数を任意長の引数に拡張するだけです。

## 道のり6. 型レベルリスト (任意長引数への拡張)

タプルで全てが解決すると思うかもしれませんが、これは上手くいきません。なぜなら、タプルはリストとは違い固定長のデータ構造だからです。

タプルのようなリストが作れたら良いのに・・・。できます！それが、型レベルリストです。

ここではゼロから自分で型レベルリストを作ることにしましょう。

---

まずは、[extensible パッケージが提供している型レベルリスト](https://www.stackage.org/haddock/lts-12.22/extensible-0.4.9/Data-Extensible-HList.html#t:HList)を使って値を作ってみましょう。(`DataKinds` と `TypeOperators` が必要です)

```hs:extensibleパッケージの型レベルリストの定義
data HList (h :: k -> *) (xs :: [k]) where
  HNil  :: HList h '[]
  HCons :: h x -> HList h xs -> HList h (x ': xs)
```

```hs:HListを使って実際に型レベルリストの値を作ってみる
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import Data.Extensible.HList

-- []
x0 :: HList Identity '[]
x0 = HNil

-- 'a' : []
x1 :: HList Identity (Maybe Char ': '[])
x1 = HCons (Identity (Just 'a')) x0

-- True : 'a' : []
x2 :: HList Identity (Bool ': Maybe Char ': '[])
x2 = HCons (Identity True) x1

-- 3 : True : 'a' : []
x3 :: HList Identity (Int ': Bool ': Maybe Char ': '[])
x3 = HCons (Identity 3) x2
```

こんな感じで型レベルリストを定義できます。(HList の H は Heterogeneous の H です。`異なる`のような意味があるそうです。)

次に定義した型レベルリストを使って何か適当な処理を行なってみたいですね。`extensible` が提供していてすぐに使えそうなのは [hlength](https://www.stackage.org/haddock/lts-12.22/extensible-0.4.9/Data-Extensible-HList.html#v:hlength) と [hfoldrWithIndex](https://www.stackage.org/haddock/lts-12.22/extensible-0.4.9/Data-Extensible-HList.html#v:hfoldrWithIndex) でしょうか。

```hs:hlengthとhfoldrWithIndexの型
hlength :: HList h xs -> Int
hfoldrWithIndex :: forall h r xs. (forall x. Membership xs x -> h x -> r -> r) -> r -> HList h xs -> r
```

`hlength` は型レベルリストの長さを計算する関数です。これは直感的に使えますね。

```hs:hlengthの具体例
> hlength x3
3
> hlength x2
2
> hlength x1
1
> hlength x0
0
```

次に `hfoldrWithIndex` です。

```hs:hfoldrWithIndexを使った関数
example :: [Int]
example = hfoldrWithIndex go [] x3
  where
    go _m x xs = const 1 (runIdentity x) : xs
```

まず型を整理しておきます。

```hs:型の整理
go :: (forall x. Membership (Int ': Bool ': Maybe Char ': '[]) x -> Identity x -> r -> r)
_m :: Membership xs x -- 現在の要素の型とリストの何番目かの位置
x  :: Identity x      -- x は型レベルリストの要素の型によって変化
xs :: r               -- foldr で畳み込まれた値
```

`x :: Identity x` なので `runIdentity :: Identity a -> a` を適用することで具体的な要素に触れることができます。(今回の例では意味無いです)

今回は `h` に `Identity` を選びましたが、仮に `Maybe` だった場合、 `x` は `x :: Maybe x` 型の値になります。

```hs:exampleの実行結果
> example
[1,1,1]
```

全然面白く無い例ではありますが、これで型レベルリストに対して何かしらの処理を適用できるようになりました。つまり、これで任意長の引数を扱えるようになったということです！

ただ、残る問題は `hfoldrWithIndex` の第一引数の関数の型です。

```hs
-- hfoldrWithIndex
(forall x. Membership xs x -> h x -> r -> r)

-- 期待する形 (制約を追加したい)
(forall x. Show => Membership xs x -> h x -> r -> r)
```

`hfoldrWithIndex` は `Show =>` というように制約を追加することができません。

最後に extensible の力を借りてこの問題を解決してみましょう。

## 道のり7. extensible のすすめ

`extensible` 利用者は `HList` をそのまま使わずに積 (Product) の形に変換してから、処理を行うことが大半です。積はタプルの一般形であり、Haskell のレコード構文を拡張したものと考えることもできます。(詳しくは TAPL を読んでみてください)

`HList` から `Product` へ変換する関数は [fromHList](https://www.stackage.org/haddock/lts-12.22/extensible-0.4.9/Data-Extensible-Product.html#v:fromHList) です。

```hs:fromHList
fromHList :: HList h xs -> h :* xs
```

実際に積に変換してみましょう。

```hs:型レベルリストから積への変換
p :: Identity :* (Int ': Bool ': Maybe Char ': '[])
p = fromHList x3
```

積は `Show` クラスのインスタンスになっているので普通に値を見ることができます。

```hs:積の形
> p
Identity 3 <: Identity True <: Identity (Just 'a') <: nil

-- (<:) = (:), nil = [] と見ればリストっぽいですね。
```

さて、最後の仕上げに先ほどの `hfoldrWithIndex` の制約バージョン [hfoldrWithIndexFor](https://www.stackage.org/haddock/lts-12.22/extensible-0.4.9/Data-Extensible-Product.html#v:hfoldrWithIndexFor) を使うことにしましょう。(この例では `TypeApplications` が必要です)

```hs:hfoldrWithIndexForを使って再定義
example :: [String]
example = hfoldrWithIndexFor c go [] p
  where
    c = Proxy @Show
    go _ x xs = show (runIdentity x) : xs
```

制約は `Proxy :: Proxy Show` のように型経由で渡します。ここでは `c = Proxy @Show` のようにしています。あとは、本当に型レベルリストを畳み込んでいるだけです。

```hs:exampleの実行結果
> example
["3","True","Just 'a'"]
```

できました！！！

おまけ

```hs:積を簡単に作る方法
-- helper
infixr 0 <::
(<::) :: Wrapper h => Repr h x -> h :* xs -> h :* (x ': xs)
(<::) = (<:) . review _Wrapper

p' :: Identity :* (Int ': Bool ': Maybe Char ': '[])
p' = 3 <:: True <:: Just 'a' <:: nil
```

`(<::)` 演算子は [(=<:)](https://github.com/fumieval/extensible/commit/adcffbb232f329e15532d0928c74820c1bc3fee5) として `extensible` に実装されました。

## 完成形

最後に任意の型レベルリストを処理できるように拡張してみます。(`FlexibleContexts` を追加する必要があります)

```hs:showArgsHListの定義
showArgsHList :: Forall Show xs => Identity :* xs -> [String]
showArgsHList = hfoldrWithIndexFor c go []
  where
    c = Proxy @Show
    go _ x xs = show (runIdentity x) : xs
```

`Forall Show xs =>` を制約に追加しました。これは型レベルリストの全て (`Forall`) の型が `Show` クラスのインスタンスであることを制約に追加しています。

```hs:showArgsHListの実行例
> showArgsHList (3 <:: True <:: Just 'a' <:: nil)
["3","True","Just 'a'"]

> showArgsHList (Right 5 <:: 3 <:: True <:: Just 'a' <:: nil)
["Right 5","3","True","Just 'a'"]

> showArgsHList (True <:: Just 'a' <:: nil)
["True","Just 'a'"]
```

どうですか！？これで当初の目標が全て達成できました！

- 引数の型が全て異なっても動いて欲しい
- `show` をリストの中に埋め込みたく無い
- 1つの関数定義で任意長の引数を扱えるようにしたい

## まとめ

- `RankNTypes`, 型レベルリスト, extensible までの道のりを一歩一歩進んでみました。
- この例を通して `extensible` パッケージやカインドに興味をもってもらえたら嬉しいです。
- extensible パッケージはもっと凄い機能がたくさんあるんだよ！
