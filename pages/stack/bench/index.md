---
title: ベンチマークの作成
date: 2019/09/15
prev: ../test/tasty.html
next: ./criterion.html
---

この章ではプログラムの実行時間の計測方法について学びます。

Haskell プログラムのベンチマークを取得するために利用するツールとして良く見るのは以下の2つです。

- [criterion](https://hackage.haskell.org/package/criterion)
- [gauge](https://hackage.haskell.org/package/gauge)

どちらも使い方はほとんど同じですが、**criterion** は HTML のレポート機能等がデフォルトで用意されており、かなり高機能です。

それに対して **gauge** は **criterion** から必要最低限の機能のみを抽出したパッケージと言えます。そのため依存関係が軽いです。また、表示結果をコンパクトにするオプションや、出力結果に色が付いたりもするのでおすすめです。

今回はどちらも使ってみましょう。

**bench** ディレクトリを作成しておきます。

```shell
$ mkdir bench
```
