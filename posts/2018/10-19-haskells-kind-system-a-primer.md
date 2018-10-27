---
title: Haskell のカインド入門 (翻訳)
author: Diogo Castro
translator: pythonissam
tags: fpcomplete, 翻訳
---

[原文](https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/)

この記事では、Haskell のカインドシステム、型とカインドの類似点について探り、それがどのようにより安全で再利用可能なコードを書くのに利用できるのかお見せしていこうと思います。

今日のメニューはこちら:

* [型とカインド]()
* [データコンストラクタと型コンストラクタ]()
* [型シグネチャとカインドシグネチャ]()
* [HOF と HKT]()
* [その他のカインド]()
  * [unboxed/unlifted 型]()
  * [制約]()
  * [データ型の昇格]()
  * [GHC.TypeLit](9
* [カインドポリモーフィズム]()
* [levity ポリモーフィズム]()

注: この記事では、GHC 8.4.3 を使っています

# 型とカインド
簡潔に言いましょう:

> 項が型に分類されるように、型もカインドに分類されます。

`"hello"` と `"world"` という値は `String` という型に分類されます。`True` や `False` といった値は `Bool` に分類されます。同じように、`String` や `Bool` といった型も `*` というカインドに分類されるのです。これは「スター」と発音します。

![State type](https://diogocastro.com/img/diagrams/kind-system001.svg)

GHCi である項の型を確認するとき `:t/:type` を使うように、ある型のカインドを確認するときは、`:k/:kind` を使うことができます。

```plain
λ> :t True
True :: Bool

λ> :k Bool
Bool :: *
```

標準の Haskell では、inhabited type (少なくとも1つの値を持つような型) のカインドは `*` です。なので、`Int`, `Int -> String`, `[Int]`,  `Maybe Int`, `Either Int Int` などのカインドは全て `*` です。なぜなら、これらの型には少なくとも1つのタームがあるからです[^1]。

全ての型が inhabited であるわけではありません。例えば `Maybe` や `Either` は inhabited type ではありません。`Maybe` 型には項がありませんが、無限ループにさえも値はないのです!

```plain
λ> x = undefined :: Maybe
<interactive>:9:18: error
    • Expecting one more argument to ‘Maybe’

λ> f x = f x :: Maybe
<interactive>:10:14: error:
    • Expecting one more argument to ‘Maybe’
```

`Maybe` や `Either` が inhabited 型ではないのなら、何なのでしょう? 答えは型コンストラクタです。

# データコンストラクタと型コンストラクタ
データを作るためにデータコンストラクタがあるように、型を作るためには型コンストラクタが用意されています。

```plain
λ> data Person = MkPerson { name :: String, age :: Int }

λ> :t MkPerson
MkPerson :: String -> Int -> Person
```

`MkPerson` は `String` 型の `name` と `Int` 型の `age` という値を受け取り、`Person` 型の別の値を生成するようなデータコンストラクタです。つまり、`MkPerson` は `String -> Int - Person` という型の値です。

```plain
λ> data Either a b = Left a | Right b

λ> :k Either
Either :: * -> * -> *
```

同じように、`Either` はカインドが `*` である2つの型 `a` と `b` を受け取り、カインドが `*` の別の型を作る型コンストラクタです。つまり、`Either` は `* -> * -> *` というカインドを持つ型です。

データコンストラクタがカリー化されて部分適用が可能になっているように、型コンストラクタにも部分適用することも可能になっています。

```plain
λ> :t MkPerson
MkPerson :: String -> Int -> Person
λ> :t MkPerson "Diogo"
MkPerson "Diogo" :: Int -> Person
λ> :t MkPerson "Diogo" 29
MkPerson "Diogo" 29 :: Person
```

![MkPerson data constructor](https://diogocastro.com/img/diagrams/kind-system002.svg)

```
λ> :k Either
Either :: * -> * -> *
λ> :k Either String
Either String :: * -> *
λ> :k Either String Int
Either String Int :: *
```

![Either type constructor](https://diogocastro.com/img/diagrams/kind-system003.svg)

# 型シグネチャとカインドシグネチャ
GHC はたいてい変数の型推論をしてくれますが、型変数のカインドも同じように正確に推論してくれます。 

```plain
-- `x` は `Bool` 型だと推論される
x = True

-- `y` は `String -> IO ()` 型だと推論される
y = putStrLn
```

```plain
-- `a` のカインドは `*` だと推論される
data List a = Cons a (List a) | Nil

-- `f` のカインドは `* -> *` だと推論される
-- `a` と `b` のカインドは `*` だと推論される
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)
```

そして変数の型を直接指定することができますが、`KingSignatures` 拡張を使うと型変数のカインドを直接指定することができるようになります。

```plain
x :: Bool
x = True

y :: String -> IO ()
y = putStrLn
```

```plain
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

data List (a :: *) = Cons a (List a) | Nil

class Functor (f :: * -> *) where
  fmap :: forall (a :: *) (b :: *). (a -> b) -> (f a -> f b)
```

分かりやすさのため、`ExplicitForAll` 拡張を使っています。これを使うとそれぞれの型変数を明示的に定義することができるようになります。

# HOF と HKT
他の関数を引数として取る高階関数 (HOF) のように、高階カインド型 (Higher-kinded types, HKT) というものもあります。これは他の型コンストラクタを引数として取るような型コンストラクタです。

```
λ> :t map
map :: (a -> b) -> [a] -> [b]
```

`map` は `(a -> b)` という型を持つ他の関数と `[a]` というリストを引数として取る関数です。

```
λ> data NonEmpty f a = MkNonEmpty { head :: a, tail :: f a }

λ> :k NonEmpty
NonEmpty :: (* -> *) -> * -> *
```

同じように、`NonEmpty` は `(* -> *)` というカインドを持つ型コンストラクタと、`*` というカインドを持つ型を受け取る型コンストラクタです。

`[]` と `Bool` に適用されたとき、`NonEmpty [] Bool` という型になりますが、こうすることでブールのリストが少なくとも1つの値を持つことが保証されます。

```
λ> :t MkNonEmpty True [False, True]
MkNonEmpty True [False, True] :: NonEmpty [] Bool
```

この型コンストラクタは適切なカインドを使っている限り、どんな2つの型にも適用することができます。例えば、`NonEmpty [] Int`, `NonEmpty Tree String`, `NonEmpty Vector Char` などです。

# その他のカインド
## unboxed/unlifted 型
inhabited 型のカインドが全て `*` だとというのは覚えていますか? ここでもう一度取り上げることにしますが、標準 Haskell では、全ての inhabited boxed (もしくは lifted) 型のカインドは `*` です。しかし、GHC の Haskell には、この法則に当てはまらない inhabited 型がいます。unboxed (もしくは unlifted) 型です。

これらの型は `ghc-prim` パッケージの `GHC.Prim` モジュールで定義されています。慣例では、全ての unlifted 型は `#` で終わるようになっており、これはマジックハッシュと呼ばれています。`MagicHash` 拡張を有効にすると使うことができます。例を挙げるなら、`Char#` や `Int#` などです。unboxed タプル `(# a, b #)` や　unboxed sum (???) `(# a | b #)` みたいなものもあるのです!

それぞれの unlifted 型は、実行時の表現を反映したカインドを持っています。これはヒープの中の何かを指したポインタなのでしょうか? それとも符号付/符号なしの1ワードサイズの値なのでしょうか? コンパイラはこの型のカインドを、どんなマシン語を生成するべきか決定する際に使います。これは kind-directed compilation と呼ばれています。

いくつか例を載せておきます:

![Unboxed types and kinds](https://diogocastro.com/img/diagrams/kind-system007.svg)

`TYPE` みたいなのはあとで説明します。

## Constraint
`=>` 矢印の左側に置くことができるものは全て `Constraint` カインドを持っていて、その中には型クラス制約も含まれています:

```plain
λ> :k Show
Show :: * -> Constraint

λ> :k Show Int
Show Int :: Constraint

λ> :k Functor
Functor :: (* -> *) -> Constraint

> :k Functor IO
Functor IO :: Constraint
```

`ConstraintKinds` 拡張を使うと、制約を第1級のものとして扱うことができます。これは驚くほど便利です。覚えているかもしれませんが、`Set` が `Functor` クラスのインスタンスを持たないことはよく知られていますね。`fmap :: (a -> b) -> f a -> f b` が 全ての型 `a` と `b` に対してうまく動きますが、`Set.map` は順番を持つ型、例えば `Ord` 制約に対してのみうまく動きます。その理由がこれです。

```plain
λ> :t Data.Set.map
Data.Set.map :: Ord b => (a -> b) -> Set a -> Set b
```

`ConstraintKinds` を使うと、存在するかどうかに関わらず、全ての制約を抽象化した、もっと一般的的な `Functor` クラスを書くことができます。

```haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Kind (Constraint)
import qualified Data.Set as Set
import qualified Data.List as List

class GFunctor (c :: * -> Constraint) (f :: * -> *) | f -> c where
  gfmap :: c b => (a -> b) -> (f a -> f b)

instance GFunctor Ord Set.Set where
  gfmap = Set.map

instance GFunctor EmptyConstraint [] where
  gfmap = List.map
```

`EmptyConstraint` は、全ての型が自明に満たすような型クラス制約です。

```haskell
{-# LANGUAGE FlexibleInstances #-}

class EmptyConstraint a
instance EmptyConstraint a
```

## データ型昇格
標準 Haskell では、`data` キーワードを使うことで自分の型や型コンストラクタを定義することができ、その後に一連のデータコンストラクタが続きます:

```haskell
data ConnectionStatus = Open | Closed
```

![ConnectionStatus type](https://diogocastro.com/img/diagrams/kind-system004.svg)

しかし、GHC の `DataKinds` 拡張を有効にすると、`data` キーワードはもう2つ別のものを作ります。カスタムカインドと、その後に来る 一連の型や型コンストラクタです。

```haskell
{-# LANGUAGE DataKinds #-}

data ConnectionStatus = Open | Closed
```

上のコードでは、新しいカインド `ConnectionStatus` が作られることになります。このカインドは、2つの uninhabited な型を持ちます。 `'Open` と `'Closed` です。

![ConnectionStatus](https://diogocastro.com/img/diagrams/kind-system005.svg)

こういった場合、`ConnectionStatus` はカインドに、`Open` と `Closed` は型に昇格した、と表現します。昇格した型 `'Open` と `'Closed` にはシングルクォートが付いていますね。このシングルクォートはほとんど常に無視することができますが、まれに[違いを明確にするために](https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html#distinguishing-between-types-and-constructors)気にする必要がある場合も出てきます。

このカスタムカインドを使って、`Open` か `Closed` にしか許可しないような型変数を定義することができます。

```haskell
{-# LANGUAGE KindSignatures #-}

data Connection (s :: ConnectionStatus) = MkConnection ...

newConnection     :: Address           -> Connection Closed
openConnection    :: Connection Closed -> Connection Open
closeConnection   :: Connection Open   -> Connection Closed
connectionAddress :: Connection s      -> Address
```

```
λ> :k Connection Int
<interactive>:1:12: error:
    • Expected kind ‘ConnectionStatus’, but ‘Int’ has kind ‘*’
```

コネクションをステータスと紐付けることで、「既に閉じたコネクションに対して `closeConnection` を呼ぶことはできない」などのルールを静的に強いることができます。

以上の手法は、私たちのカスタムデータ型以外にも使うことができます。例えば、`DataKinds` を有効にした状態で `Bool` があるとき、同じようにカインドに昇格させてみましょう。

```plain
λ> data F (b :: Bool) = MkF deriving Show

λ> MkF :: F 'True
MkF

λ> MkF :: F 'False
MkF

λ> MkF :: F 'Open
<interactive>:30:10: error:
    • Expected kind ‘Bool’, but ‘ 'Open’ has kind ‘ConnectionStatus’
```

# GHC.TypeLits
GHC はその他2つのとても便利で独創的なカインドを提供してくれています。これは base の `GHC.TypeLits` モジュールに隠されています。

まず `Symbol` です。これは、型レベルの文字列のためのカインドです。これを使うことで、`"hello"` などの文字列リテラルを型として扱うことが可能になります。

![Symbol kind](https://diogocastro.com/img/diagrams/kind-system006.svg)

こうすることで、型を文字列リテラルでタグ付けすることができます。

```haskell
-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits (Symbol)
import Data.Ratio ((%))

newtype Money (currency :: Symbol) = Money Rational

fivePence :: Money "GBP"
fivePence = Money (5 % 100)

twoEuros :: Money "EUR"
twoEuros = Money 2
```

項レベル (`data Money = Money String Rational`) ではなく、型レベルで通貨を表現することで、異なる通貨が混ざることはないことを静的に保証できます。例えば、EUR (ユーロ) と GBP (ポンド) を足すことはできません。

```haskell
add :: Money c -> Money c -> Money c
add (Money x) (Money y) = Money (x + y)
```

```plain
λ> add fivePence fivePence
Money (1 % 10)

λ> add fivePence twoEuros
<interactive>:8:15: error:
    • Couldn't match type ‘"EUR"’ with ‘"GBP"’
```

値レベルの通貨だったら、実行時にチェックして、全ての関数に `Maybe` や `Either`、`MonadError e m` 等を返させることで、失敗の可能性を送信する必要があるでしょう。

少しおもしろいのは、これが単なる例ではなく、まさに safe-money ライブラリが [dense monetary value](https://hackage.haskell.org/package/safe-money-0.7/docs/Money.html#t:Dense) の表現に使っている手法であることです!

お金について計算する必要がないときは、値を `Rational` として保存しておくのは少しやりすぎ感があります。こういった場合のために、このライブラリは別の [`Discrete`](https://hackage.haskell.org/package/safe-money-0.7/docs/Money.html#t:Discrete-39-) 型を提供しています。これはシンプルな `Integer` のラッパーです。

ただ、問題があります。[ブログの紹介記事](https://ren.zone/articles/safe-money#scales)でも説明されている理由から、お金の値を整数として表現するとき、通貨だけを追跡するのは十分ではありません。その単位の比率も同時に追跡する必要があります。ドルを扱うときは、単位の比率は1対1ですが、セントを扱うときは100対1になりますよね (なぜなら、1ドルは100セントだから)。

この単位の比率を型で表現しようとするのなら、私たちには `Nat` が必要です。これは型レベルの自然数のカインドであり、`GHC.TypeLits` からエクスポートされています。これを使うと `Symbol` と同じように、数値リテラルを型として扱うことができるようになります。

```haskell
{-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits (Symbol, Nat)

newtype Discrete (currency :: Symbol) (scale :: (Nat, Nat))
  = Discrete Integer

oneDollar :: Discrete "USD" '(1, 1)
oneDollar = Discrete 1

oneDollarThirtyCents :: Discrete "USD" '(100, 1)
oneDollarThirtyCents = Discrete 130
```

えぇ、これは問題ありません。`scale :: (Nat, Nat)` の中の `(,)` は、`DataKinds` によって昇格されたタプル型です。そして、`'(100, 1)` の `(,)` は、型コンストラクタに昇格されたタプルのデータコンストラクタです。

# カインドポリモーフィズム
パラメトリック多相は、Haskell の世界ではどこにでも見られます。型の抽象化を助けるものですね。`PolyKinds` 拡張を使えば、カインドに対してもより多相的な抽象化を行うことができますよ!

例を見てみましょう。

```haskell
data Proxy a = MkProxy
```

`Proxy` は型変数 `a` を持っています。1つデータコンストラクタがあるだけで、引数はありませんね。こいつは型変数をタグ付けして使います。手持ちの値がないとき、型を渡したりするのに `Proxy` はとても役に立ちます[^2]。

```haskell
λ> intRepresentative = MkProxy :: Proxy Int

λ> stringRepresentative = MkProxy :: Proxy String
```

問題は、デフォルトで GHC が `a` のカインドを `*` だと解釈してしまうことで、これはつまり、`Proxy` が全ての型について動くわけではないことを意味しています。lifted な型に対してのみなのです。

```plain
λ> :k Proxy
Proxy :: * -> *

λ> maybeRepresentative = MkProxy :: Proxy Maybe
<interactive>:9:38: error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’
```

普通は、以下のように `Proxy` のような型をいくつも作らないといけません:

```haskell
data Proxy a = MkProxy
data Proxy1 (a :: * -> *) = MkProxy1
data Proxy2 (a :: * -> * -> *) = MkProxy2
data HKProxy1 (a :: (* -> *) -> * -> *) = MkHKProxy1

stringRepresentative   = MkProxy    :: Proxy String
maybeRepresentative    = MkProxy1   :: Proxy1 Maybe
eitherRepresentative   = MkProxy2   :: Proxy2 Either
nonEmptyRepresentative = MkHKProxy1 :: HKProxy1 NonEmpty
```

この方針を続けていくのは、明らかに不可能でしょう。しかし、`PolyKinds` 拡張を有効にすれば、全ての型 `a` とカインド `k` について動く `Proxy` を作ることができます。

```haskell
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

data Proxy (a :: k) = MkProxy

-- Now `a` can be anything at all
maybeRepresentative    = MkProxy :: Proxy Maybe
nonEmptyRepresentative = MkProxy :: Proxy NonEmpty
functionRepresentative = MkProxy :: Proxy (->)
helloRepresentative    = MkProxy :: Proxy "hello"
showRepresentative     = MkProxy :: Proxy Show
functorRepresentative  = MkProxy :: Proxy Functor
openRepresentative     = MkProxy :: Proxy 'Open
```

実は、このカインドシグネチャを完全に無くすことができて、その場合も GHC は `a :: k ` だと推論してくれます。

```haskell
λ> :set -XPolyKinds
λ> data Proxy a = MkProxy

λ> :k Proxy
Proxy :: k -> *
```

実世界で使われているカインドポリモーフィズムの別の例を挙げるなら、Servant に存在しています。これは型安全な web API を書くためのライブラリです。このライブラリには `:>` という型があって、これは API のコンポーネントを組み合わせて、完全なエンドポイントにするのに使われます。これらのコンポーネントの型とカインドは、それぞれ異なるものになります。

```haskell
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

data (:>) (a :: k) (b :: *)
infixr 4 :>
```

以下は `POST /books` にレスポンスを返すエンドポイントを記述したものです。`"books" :: Symbol` と `ReqBody '[JSON] Book :: *`、`Post '[JSON] () :: *` という型を組み合わせるのに、`:>` を使っています。

```haskell
type BooksAPI = "books" :> ReqBody '[JSON] Book :> Post '[JSON] ()
```

# `Levity ポリモーフィズム`
いくつかのケースでは、lifted と unlifted の型どちらに対しても使えるように、抽象化するのが便利かもしれません。例えば、`error` "関数"を考えてみましょう。これはエラーメッセージを受け取って、例外を投げる関数です:

```haskell
{-# LANGUAGE ExplicitForAll #-}

error :: forall a. String -> a
```

`error` の返り値は多相型になっていて、(ほとんど) どこでも使うことができます。

```haskell
increment :: Int -> Int
increment x = error "oops"
```

しかし、`a` は (デフォルトで) `*` というカインドの型だと解釈されているので、unlifted な型が来るような場所ではこいつを使うことができません!

```haskell
incrementUnlifted :: Int# -> Int#
incrementUnlifted x = error "oops"
```

```plain
<interactive>:22-40: error
   • Couldn't match a lifted type with an unlifted type

```

新しい知識で武装した私たちが、まず最初にやることは、`PolyKinds` を有効にして、カインド変数 `k` を導入することかもしれませんね。しかし、これもうまくいきません。

```haskell
error :: forall k (a :: k). String -> a
```

```plain
/Playground.hs:15:40: error:
    • Expected a type, but ‘a’ has kind ‘k’
    • In the type signature: error :: forall k (a :: k). String -> a
```

コンパイルが通らなかったのは、`a :: k` が inhabited な型以外の別の型になる意味が分からないと判断されたからです。`k` が `Symbol` で、`a` が `"hello"` だということにしてみましょう。もしも `"hello"` 型が uninhabited だった場合、この関数はどんな値を返すのでしょうか?

`error` 関数を lifted と unlifted に含まれる全ての inhabited な型に対して動くようにするためには、levity 多相というものが必要になります。

トリックは、私たちが先ほど見た [`TYPE r`](https://hackage.haskell.org/package/base-4.11.1.0/docs/GHC-Exts.html#t:TYPE) にあります。このカインドは、`r :: RuntimeRep` に対して多相化されています。これは型の実行時の表現を記述しており、以下のどれか1つになります:

```haskell
data RuntimeRep = VecRep VecCount VecElem   -- ^ a SIMD vector type
                | TupleRep [RuntimeRep]     -- ^ An unboxed tuple of the given reps
                | SumRep [RuntimeRep]       -- ^ An unboxed sum of the given reps
                | LiftedRep       -- ^ lifted; represented by a pointer
                | UnliftedRep     -- ^ unlifted; represented by a pointer
                | IntRep          -- ^ signed, word-sized value
                | WordRep         -- ^ unsigned, word-sized value
                | Int64Rep        -- ^ signed, 64-bit value (on 32-bit only)
                | Word64Rep       -- ^ unsigned, 64-bit value (on 32-bit only)
                | AddrRep         -- ^ A pointer, but /not/ to a Haskell value
                | FloatRep        -- ^ a 32-bit floating point number
                | DoubleRep       -- ^ a 64-bit floating point number
```

`TYPE 'IntRep` がどんな unlifted な整数のカインドなのかは分かりましたよね? 他にも、`TYPE 'FloatRep` は unlifted floats になって... みたいな感じです。しかし、なんか目立つやつがいます。`TYPE 'LiftedRep` です。これは全ての lifted 型のカインドなのです。実は、私たちが今まで使ってきた `*` というカインドは、[`TYPE 'LiftedRep` のシノニム以外の何者でもありません](https://hackage.haskell.org/package/ghc-prim-0.5.2.0/docs/GHC-Types.html#t:-42-)!

なので、これで `TYPE r` を全ての unlifted 型に対して抽象化するだけではなく、lifted な型に対しても抽象化することができることが分かりました。

```haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

import GHC.Exts (TYPE, RuntimeRep)

error :: forall (r :: RuntimeRep) (a :: TYPE r). String -> a
```

それで、`error` を `increment` でも `incrementUnlifted` からでも使えるようにもなりました。

Levity 多相にも制限があります。使うことができない場所があるのです。それがどこでなぜ使えないのか知りたいのなら、Richard Eisenberg によるこのテーマの[すばらしいお話](https://www.youtube.com/watch?v=lSJwXZ7vWBw)を観てみてください。

# まとめ
今まで、当然ですが、こう不思議に思った方もいるかもしれません。一体何の話だったんだ? なんでカインドなんていうシステムがあって、なんでカインドを気に掛ける必要があるんだ? ってね。えーっと、Java や C# など、型を全く分類することができないような言語では、全ての型変数 `<T>` は `*` というカインドに含まれています。`Functor<List>` のように、`* -> *` というカインドの型変数を持つことができないのです。カインドについて語ることができなければ、ファンクターやモナド、もしくは `NonEmpty f a` という、私たちがさっき見たデータ型に存在するような抽象さえ定義することができなくなります。さらに、levity 多相を使うことで、[ここにある `Num a` クラスの一般的なバージョン](https://gist.github.com/dcastro/a7f9730981fa404415588224350dc918)のように、boxed 型でも unboxed 型でも使えるような抽象を書くことができますが、Java のような言語では、総称的型変数は boxed 型にしかなれません。

カインドシステムを持つことで、型レベルプログラミングの世界へのドアも開けてきます (次のブログ記事でもっと掘り下げようと思っていますが)。こうすることで、extensible レコードや、Servant の型レベルの web API、それに `Connection s` や `Money c` のような、ただ単純に、一般に安全な API を定義することも可能になります。

最先端の研究を探してみると、SImon Peyton Jones は最近、[Haskell に線型性を与えることを議論しています](https://www.youtube.com/watch?v=t0mhvd3-60Y) (???)。これは私の想像ですが、関数の型 `(->) a b` が、追加で新しいカインド `Linearity` の型変数を持つようになって、`Omega` と `One` が組み合わせられることになる、ということだと思います。(???)

この他にありそうな質問は、項が型に分類されて型がカインドに分類されるのなら、カインドも何かに分類されるの? というものでしょう。えー、GHC 7 を含むそれ以前のバージョンでは、カインドは __sort__ に分類されていました。全てのカインドは[ユニークな sort である `BOX`](https://downloads.haskell.org/~ghc/7.6.1/docs/html/users_guide/promotion.html) を持ち、GHC のインターナルになっていて、私たち開発者からは見えないようになっていました。

しかしバージョン8から、GHC は方向性を変えます。気づいている人もいると思いますが、この記事で私は、型とカインドの類似性について説明しようとしてきました。どちらも高階で、多相で、推論ができて、カリー化されていて、等々、これは偶然ではなかったのです! `TypeInType` の進展により、GHC 8 である拡張が導入され、型とカインド (それに sort) は1つに統合され、同じものになりました! ここまで来ると、型を他の型に分類することができるようになります (???)。`3` は `Int` 型に、`Int` 型は `*` に、`*` は `*` 型に分類されます。この統合によって、完全な依存型への道が開かれたことになります[^3]。

また、より最新のドキュメントを見ると、`*` というカインドが `Type` になっているのに気づくかもしれません (`TYPE r` と混同しないように)。こいつらは今のところ[シノニム](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Kind.html#t:Type)になっていますが、徐々に `*` から `Type` を使うように移行される予定です。

1. 特筆すべき例外は `data Void` で、作ることができません。それ自体に値が存在しないのです。そして、`undefined` や `f x = f x` の無限ループのように、[絶対に成功することのない項](https://wiki.haskell.org/Bottom)を持っています。
2. `Proxy` は base の `Data.Proxy` モジュールにすでに定義されています。ここでは、こいつのカインドの裏にあるロジックをお見せするために、独自の定義をしています。`Data.Proxy` とその使い方についてもっと知りたいのなら、[Kwang Seo のブログ記事](https://kseo.github.io/posts/2017-01-15-data-proxy.html)をチェックしてみてください。
3. もっと依存型について学習したいのなら、[Type-Driven Development with Idris](https://www.manning.com/books/type-driven-development-with-idris) や、最近出版された [The Little Typer](https://mitpress.mit.edu/books/little-typer) について読んでみることをおすすめします。
