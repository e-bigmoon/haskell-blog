---
title: NoStarIsType 言語拡張が必要になるとき
author: Shinya Yamaguchi
tags: bigmoon
---

## はじめに

1つ前の GHC-8.6 から遭遇するかもしれないエラーの話です。

```haskell
λ ghc -V
The Glorious Glasgow Haskell Compilation System, version 8.8.1

λ :kind! 10*10
<interactive>:1:1: error:
    • Expected kind ‘* -> Nat -> k0’, but ‘10’ has kind ‘Nat’
    • In the type ‘10 * 10’
```

```haskell
λ :set -XNoStarIsType
λ :kind! 10*10
10*10 :: Nat
= 100
```

<!--more-->

## 型レベル四則演算

[GHC.TypeLits](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html) に用意されている関数を使って型レベル自然数の四則演算を行ってみましょう。

```haskell
λ :set -XDataKinds -XTypeOperators
λ import GHC.TypeLits

λ :kind! 1+1
1+1 :: Nat
= 2

λ :kind! 10-1
10-1 :: Nat
= 9

λ :kind! Div 10 2
Div 10 2 :: Nat
= 5

λ :kind! 10 * 10
<interactive>:1:1: error:
    • Expected kind ‘* -> Nat -> k0’, but ‘10’ has kind ‘Nat’
    • In the type ‘10 * 10’
```

掛け算だけエラーになりましたね・・・。それぞれの演算子のカインドを確認してみましょう。

```haskell
λ :k (+)
(+) :: Nat -> Nat -> Nat
λ :k (-)
(-) :: Nat -> Nat -> Nat
λ :k Mod
Mod :: Nat -> Nat -> Nat
λ :k (*)
(*) :: *
```

1つだけ変ですね。これは `*` が **Bool** や **Maybe** などのよくある基本的な型 (lifted boxed types) のカインドの記号として割り当てられているためです。

```haskell
λ :k Bool
Bool :: *

λ :k Maybe
Maybe :: * -> *
```

[GHC 8.6](https://gitlab.haskell.org/ghc/ghc/wikis/migration/8.6) から **StarIsType** 言語拡張がデフォルトで有効になり、`*` カインドは `Type` カインドのシノニムとして定義されるようになりました。なので明示的に **StarIsType** を無効にすると直ります。(この辺りの話題については既に ["TypeOperators => NoStarIsType"の延期の提案](https://www.reddit.com/r/haskell_jp/comments/8t8p4j/typeoperators_nostaristype%E3%81%AE%E5%BB%B6%E6%9C%9F%E3%81%AE%E6%8F%90%E6%A1%88/) などにまとまっているため、気になる方はご参照ください)

```haskell
λ :set -XNoStarIsType

λ :k Bool
Bool :: Type

λ :k Maybe
Maybe :: Type -> Type

λ :k (*)
(*) :: Nat -> Nat -> Nat
```

ということでこれで無事に型レベルの掛け算ができるようになりました。

```haskell
λ :kind! 10 * 10
10 * 10 :: Nat
= 100
```

## まとめ

`kind!` のエイリアスとして `k!` コマンド欲しい。

## 参考リソース

- [StarIsType - Glasgow Haskell Compiler User's Guide](https://downloads.haskell.org/~ghc/8.8.1/docs/html/users_guide/glasgow_exts.html?highlight=nostaristype#extension-StarIsType)
- [GHC 8.6.x Migration Guide](https://gitlab.haskell.org/ghc/ghc/wikis/migration/8.6)
- ["TypeOperators => NoStarIsType"の延期の提案](https://www.reddit.com/r/haskell_jp/comments/8t8p4j/typeoperators_nostaristype%E3%81%AE%E5%BB%B6%E6%9C%9F%E3%81%AE%E6%8F%90%E6%A1%88/)
