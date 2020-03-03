---
title: テストの作成
published: 2017/12/25
updated: 2019/09/15
prev: ../doc/haddock-settings.html
next: ../test/hspec.html
---

Haskell のテストライブラリの代表的なものとして [hspec](https://hackage.haskell.org/package/hspec), [tasty](https://hackage.haskell.org/package/tasty), [doctest](https://hackage.haskell.org/package/doctest), [QuickCheck](https://hackage.haskell.org/package/QuickCheck), [HUnit](https://hackage.haskell.org/package/HUnit) などがあります。

それぞれを一言で説明すると、

- **hspec** や **tasty** はテストフレームワークです。
- **HUnit** は単体テストを記述するためのパッケージです。
- **QuickCheck** は単体テストでは想定していなかったエッジケースをプログラムが生成するランダムな入力によってあぶり出すことができます。
- **doctest** ではドキュメントに記載されている内容をテストすることで、正しい内容であることを保証します。

**QuickCheck** を使えば、関数をリファクタリングする際にリファクタリング前と後の関数が同一であるという性質をテストできるようになります。

例えば、**minfree** と改良後の **minfree'** がランダムな入力に対して、同様の結果を返すことで、関数の性質が変化していないことをチェックします。

## Haskell でもテストは必要？

**Haskell** は型がテストのように振る舞い、結果としてバグが少ないプログラムを素早く作ることができます。

しかしながら、それでもテストを書くことは重要です。

なぜなら、値については何も保証しないからです。コンパイルが通ったからといって全てが正しいわけではありません。

例えば、以下の関数はコンパイルには通りますが、期待通りの結果を返さないでしょう。

```haskell
isZero :: Int -> Bool
isZero 1 = True
isZero 0 = False
```

**Haskell** においてこの種のバグをデバッグすることは、基本的に難しい作業ではないでしょうか。(現実的には、モナドトランスフォーマーのベースに **IO** があって **print** デバッグ的なことができたり、**Debug.Trace** の **trace** 関数を使ったりして実際の値を確かめたりします。)

このようなバグができるだけ入り込まないよう、関数が仕様を満たしていることを確認するために単体テストやランダムテストを実施します。
