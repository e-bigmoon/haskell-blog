---
title: テストの作成
date: 2019/09/14
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
