---
title: Haskell のカインド入門 (翻訳)
author: Diogo Castro
translator: pythonissam
tags: fpcomplete, 翻訳
---

この記事では、Haskell のカインド、型とカインドの類似点について探り、それがどのようにより安全で再利用可能なコードを書くのに利用できるのかお見せしていこうと思います。

今日のメニューはこちら:

* 型とカインド
* データコンストラクタと型コンストラクタ
* 型シグネチャとカインドシグネチャ
* HOF と HKT
* その他のカインド
  * unboxed/unlifted 型
  * 制約
  * データ型の昇格
  * GHC.TypeLit
* 多相カインド
* levity 多相

注: この記事では、GHC 8.4.3 を使っています

# 型とカインド
簡潔に言いましょう:

> 値 (???) が型に分類されるように、型もカインドに分類されます。

`"hello"` と `"world"` という値は `String` という型に分類されます。`True` や `False` といった値は `Bool` に分類されます。同じように、`String` や `Bool` といった型も `*` というカインドに分類されるのです。これは「スター」と発音します。

![State type](https://diogocastro.com/img/diagrams/kind-system001.svg)

GHCi であるタームの型を確認するとき `:t/:type` を使うように、ある型のカインドを確認するときは、`:k/:kind` を使うことができます。

```plain
λ> :t True
True :: Bool

λ> :k Bool
Bool :: *
```

標準の Haskell のでは、inhabited 型 (???) (少なくとも1つの値を持つような型) のカインドは `*` です。なので、`Int`, `Int -> String`, `[Int]`,  `Maybe Int`, `Either Int Int` などのカインドは全て `*` です。なぜなら、これらの型には少なくとも1つのタームがあるからです[^1]。

全ての型が inhabited であるわけではありません。例えば `Maybe` や `Either` は inhabited な型ではありません。`Maybe` 型には値がありませんが、無限ループさえもないのです!

```plain
λ> x = undefined :: Maybe
<interactive>:9:18: error
    • Expecting one more argument to ‘Maybe’

λ> f x = f x :: Maybe
<interactive>:10:14: error:
    • Expecting one more argument to ‘Maybe’
```

`Maybe` や `Either` が inhabited 型ではないのなら、何なのでしょう? 答えはタイプコンストラクタです。

# データコンストラクタと型コンストラクタ
Just like we have data constructors for creating data, we also have type constructors for creating types.

値を作るためにデータコンストラクタがあるように、型を作るためには型コンストラクタが用意されています。

λ> data Person = MkPerson { name :: String, age :: Int }

λ> :t MkPerson
MkPerson :: String -> Int -> Person
MkPerson is a data constructor that, given two values name and  age of type String and Int, creates another value of type  Person. In other words, MkPerson is a value of type  String -> Int -> Person.

λ> data Either a b = Left a | Right b

λ> :k Either
Either :: * -> * -> *
Similarly, Either is a type constructor that, given two types a and b of kind *, creates another type of kind *. In other words,  Either is a type of kind * -> * -> *.

Just like data constructors are curried and can be partially applied, so can type constructors.
