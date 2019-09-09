---
title: AST を拡張しよう
author: Shinya Yamaguchi
tags: bigmoon, extensible, package
---

## はじめに

実験的な内容です。(@fumievalさん, @matsubara0507さん、アドバイスありがとうございました)

<!--more-->

## モチベーション

まずは、以下のような型 `Expr` と関数 `pretty` が定義されているとします。

```haskell
data Expr
  = Constant Int
  | Add Expr Expr
  deriving (Eq, Show)

pretty :: Expr -> String
pretty (Constant i) = show i
pretty (Add e1 e2) = pretty e1 <> " + " <> pretty e2
```

今回、この `Expr` をベースとして新しい型 `ExprM`, `ExprS` をそれぞれ定義したい場合、どのように書けば良いのでしょうか？

素朴に定義するとなると、以下のようになりそうです。

```haskell
data ExprM
  = Constant Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show)

pretty :: Expr -> String
pretty (Constant i) = show i
pretty (Add e1 e2) = pretty e1 <> " + " <> pretty e2
pretty (Mul e1 e2) = pretty e1 <> " * " <> pretty e2
```

```haskell
data ExprS
  = Constant Int
  | Add Expr Expr
  | Sub Expr Expr
  deriving (Eq, Show)

pretty :: Expr -> String
pretty (Constant i) = show i
pretty (Add e1 e2) = pretty e1 <> " + " <> pretty e2
pretty (Sub e1 e2) = pretty e1 <> " - " <> pretty e2
```

しかし、同じようなコードが含まれていて冗長なので何とかしたいです・・・。

## 方針

- 各コンストラクタ `Constant`, `Add` 等を `extensible` のフィールドとして定義
- `Expr`, `ExprS`, `ExprM` は、それぞれのフィールドを集めて作った拡張可能和として定義
- スマートコンストラクタは再利用できるようにしたい

## 実装

1. コンストラクタの定義
1. 型の定義
1. スマートコンストラクタの定義
1. pretty 関数の定義
1. 新しい型を定義

### コンストラクタの定義

まずはそれぞれのフィールドを定義しましょう。それぞれの型は `Assoc Symbol Type` のカインドを持ちます。

```haskell
-- Field/Constant.hs
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import Data.Extensible
type Constant = "constant" >: Int
```

```haskell
-- Field/Add.hs
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import Data.Extensible
type Add expr = "add" >: (expr, expr)
```

これらのフィールドは以下のような型のコンストラクタを1つずつ切り出したような感じです。

```haskell
data Expr
  = Constant Int
  | Add Expr Expr
```

### 型の定義

フィールドの定義は完了したので、次にそれらのフィールドを集めて型にしましょう。

拡張性を得るために型クラスを定義します。

```haskell
-- Expr.hs
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Base where

import Data.Extensible
import Data.Kind
import GHC.TypeLits

class Expr expr where
  type FieldList expr :: [Assoc Symbol Type]
  liftExpr :: Variant (FieldList expr) -> expr
```

`FieldList expr` によって型に応じてフィールドが変化します。

実際に `Expr` 型を定義してみましょう。

```haskell
-- Expr/Base.hs
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Expr.Base where

import Expr
import Field.Add
import Field.Constant

import Data.Extensible

newtype ExprB = ExprB
  { unwrapExprB :: Variant ExprBFields
  } deriving (Eq, Show)

type ExprBFields = '[ Constant, Add ExprB ]

instance Expr ExprB where
  type FieldList ExprB = ExprBFields
  liftExpr = ExprB
```

- `type ExprBFields` は型に含まれるフィールドを表します。
- 再帰的に定義するために `newtype ExprB` を宣言しています。
- `Variant ExprBFields` によって `Constant`, `Add ExprB` の直和型っぽい感じになります。

`liftExpr` は再利用可能なスマートコンストラクタを作るためにあります。次で説明します。

### スマートコンストラクタの定義

ここまでで型の定義は終わりました。次は値を作りましょう。

```haskell
-- Field/Add.hs
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Field.Add where

import Expr

import Control.Lens ((#))
import Data.Extensible

type Add expr = "add" >: (expr, expr)

add e1 e2 = liftExpr (#add # (e1, e2))
```

`liftExpr` は文脈に応じて適切なタグに変化します。例えば `ExprB` 型であれば `ExprB` コンストラクタになります。

同様に `Constant` の値を作る関数も定義しましょう。

```haskell
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Field.Constant where

import Expr

import Control.Lens ((#))
import Data.Extensible

type Constant = "constant" >: Int

c i = liftExpr (#constant # i)
```

実際に使ってみるとこんな感じです。

```haskell
> add (c 10) (c 20) :: ExprB
ExprB {unwrapExprB = EmbedAt $(mkMembership 1) (add @= (ExprB {unwrapExprB = EmbedAt $(mkMembership 0) (constant @= 10)},ExprB {unwrapExprB = EmbedAt $(mkMembership 0) (constant @= 20)}))}
```

extensible の形式で表示されていますが、問題無く値が作れています。型注釈が無い場合はコンパイルエラーになってしまいますが、ここでは気にしないことにします。

### pretty 関数の定義

値が作れるようになったら、次は `pretty` 関数を作ります。

ここが一番面白いポイントだと思っているのですが、`pretty` のような関数を各フィールドに対して動作する型クラスのメソッドとして定義します。

まずは、パターンマッチのための補助関数 `matchVariant` を定義します。

```haskell
-- Expr.hs
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
...

import Data.Functor.Identity

...

matchVariant :: forall c xs r. Forall c xs
  => Proxy c -> (forall x. c x => Membership xs x -> TargetOf x -> r) -> Variant xs -> r
matchVariant _ f = matchField $ htabulateFor (Proxy @c) $ \m -> Field $ Match $ f m . runIdentity
```

次に `pretty` 関数を定義するための型クラスを作ります。

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Operation.Pretty where

import Expr

import Data.Extensible

pretty' :: Forall PrettyField xs => Variant xs -> String
pretty' = matchVariant (Proxy @PrettyField) prettyField

class Expr expr => Pretty expr where
  pretty :: expr -> String

class PrettyField kv where
  prettyField :: proxy kv -> TargetOf kv -> String
```

実際にインスタンスを定義します。

```haskell
-- Field/Add.hs
...
{-# LANGUAGE FlexibleInstances #-}
...
import Operation.Pretty
...

instance Pretty expr => PrettyField (Add expr) where
  prettyField _ (l, r) = pretty l <> " + " <> pretty r
```

```haskell
-- Field/Constant.hs
...
{-# LANGUAGE FlexibleInstances #-}
...
import Operation.Pretty
...

instance PrettyField Constant where
  prettyField _ = show
```

`ExprB` に対する定義はボイラープレートのようなものです。

```haskell
-- Expr/Base.hs
...
import Operation.Pretty
...

instance Pretty ExprB where
  pretty = pretty' . unwrapExprB
```

実際に使ってみます。

```haskell
> e1 = add (c 10) (c 20) :: ExprB
> pretty e1
"10 + 20"
```

期待通り、ちゃんと動いています。

### 新しい型を定義

最後に既存の型を拡張して `ExprM` を作ります。

まずは `Mul` フィールドの `pretty` とコンストラクタの定義を追加しましょう。

```haskell
-- Field/Mul.hs
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Field.Mul where

import Expr
import Operation.Pretty

import Control.Lens ((#))
import Data.Extensible

type Mul expr = "mul" >: (expr, expr)

instance Pretty expr => PrettyField (Mul expr) where
  prettyField _ (l, r) = pretty l <> " * " <> pretty r

mul e1 e2 = liftExpr (#mul # (e1, e2))
```

次に `ExprM` 型を定義します。

```haskell
-- Expr/Mul.hs
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Expr.Mul where

import Expr
import Expr.Base
import Field.Add
import Field.Mul
import Field.Constant
import Operation.Pretty

import Data.Extensible

newtype ExprM = ExprM
  { unwrapExprM :: Variant ExprMFields
  } deriving (Eq, Show)

type ExprMFields = '[ Constant, Add ExprM, Mul ExprM ]

instance Expr ExprM where
  type FieldList ExprM = ExprMFields
  liftExpr = ExprM

instance Pretty ExprM where
  pretty = pretty' . unwrapExprM
```

ほとんど同じですが、`type ExprMFields` の部分で `Mul ExprM` を追加しています。(`ExprB` のフィールドに単純に追加する方法も一応可能です。)

実際に使ってみると、ちゃんと異なる型と認識してコンパイルエラーになってくれます。

```haskell
> e1 = add (c 10) (c 20) :: ExprB
> e2 = add (c 40) (c 50) :: ExprM

> add e1 e2
-- 型が異なるためコンパイルエラー

> pretty e1
"10 + 20"
> pretty e2
"40 + 50"

> e3 = mul (c 60) (c 70) :: ExprB
-- ExprB 型には Mul フィールドが存在していないため、コンパイルエラー

> e3 = mul (c 60) (c 70) :: ExprM
> pretty e3
"60 * 70"
```

## おまけ

### 既存のフィールドを拡張する

`UndecidableInstances` 拡張を使っても良ければ、以下のように `ExprBFields ++ '[ Mul ExprM ]` と書くこともできます。

```haskell
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Expr.Mul where

import Expr
import Expr.Base
import Field.Mul
import Operation.Pretty

import Data.Extensible

newtype ExprM = ExprM
  { unwrapExprM :: Variant ExprMFields
  } deriving (Eq, Show)

type ExprMFields = ExprBFields ++ '[ Mul ExprM ]
-- type ExprMFields = '[ Constant, Add ExprM, Mul ExprM ]

instance Expr ExprM where
  type FieldList ExprM = ExprMFields
  liftExpr = ExprM

instance Pretty ExprM where
  pretty = pretty' . unwrapExprM
```

### 操作を追加しよう

`eval` を追加してみましょう。

```haskell
-- Operation/Eval.hs
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Operation.Eval where

import Expr

import Data.Extensible

eval' :: Forall EvalField xs => Variant xs -> Int
eval' = matchVariant (Proxy @EvalField) evalField

class Eval expr where
  eval :: expr -> Int

class EvalField kv where
  evalField :: proxy kv -> TargetOf kv -> Int
```

さらに、インスタンス定義もこのファイルに定義します。

```haskell
-- Operation/Eval.hs
...
{-# LANGUAGE FlexibleInstances #-}
...

import Expr.Base
import Expr.Mul
import Field.Add
import Field.Constant
import Field.Mul

...

instance Eval ExprB where
  eval = eval' . unwrapExprB

instance Eval ExprM where
  eval = eval' . unwrapExprM

...

instance EvalField Constant where
  evalField _ = id

instance Eval expr => EvalField (Add expr) where
  evalField _ (l, r) = eval l + eval r

instance Eval expr => EvalField (Mul expr) where
  evalField _ (l, r) = eval l * eval r
```

これで使えるようになりました。

```haskell
> e1 = add (c 10) (c 20) :: ExprB
> e2 = mul (c 40) (c 50) :: ExprM

> eval e1
30
> eval e2
2000
```

## まとめ

何かに使えないかなー。
