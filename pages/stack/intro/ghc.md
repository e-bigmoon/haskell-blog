---
title: GHC について
published: 2019/09/14
# updated: 2019/09/14
prev: ./repl.html
next: ./package-and-deps.html
---

**ghc** は **Glasgow Haskell Compiler** の意味です。 **GHC** はイギリスのグラスゴー大学で開発された **Haskell** コンパイラなのでこのような名前がついています。注意して欲しい点としては **Haskell == GHC** と理解してしまうことです。

**Haskell** というプログラミング言語の仕様が **Haskell98**, **Haskell2010** と続いてきたなかで実際にこの仕様に沿ったプログラムを実行できるコンパイラ (処理系) の一つが **GHC** というのが正しい理解です。他にも [UHC](https://wiki.haskell.org/UHC) や [LHc](https://github.com/Lemmih/lhc) などがあるそうです。([Implementations](https://wiki.haskell.org/Implementations))

また、**Haskell2010** の仕様から外れる機能等については **GHC拡張** として実装されています。

処理系が複数あると言っても主流 (デファクトスタンダード) は **GHC** です。なので、基本的に **Haskell** の話をする時はみんな頭の中に **GHC** がインストールされていると思いますし、実際の開発において **GHC** 以外を採用することは一般的に考えにくいです。
