---
title: Haskell で非同期例外処理を扱う
author: Michael Snoyman
translator: pythonissam
tags: fpcomplete, 翻訳
---

Original post: [Asynchronous Exception Handling in Haskell](https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell)

![Exception Handling Text Small](https://www.fpcomplete.com/hubfs/Blog/Exception%20Handling%20Text%20Small.jpg?t=1526342142799)

先週、私は Haskell で非同期例外処理をする、というタイトルでウェビナーをやりました。まだ見ていないのなら、[この動画](https://www.fpcomplete.com/blog/how-to-handle-asynchronous-exceptions-in-haskell)を見ることをおすすめします。[スライドも公開しています](https://www.snoyman.com/reveal/async-exception-handling)。

私にとって[習慣になりつつある](https://www.snoyman.com/blog/2017/12/what-makes-haskell-unique)のですが、この話のためにスライドを作る前に、ブログ記事を書く要領で内容を構成しました。テキストベースの学習が好きな人のために、その記事をここに置いておきます。

<!--more-->

実行時の例外は今日の多くのプログラミング言語でありふれたものです。これは諸刃の剣です。全ての関数の返り値をチェックする必要がなくなり、正しいコードが書きやすくなることはあります (たぶん)。一方で、コードがどこで処理を抜けたのか隠してしまい、リソースの整理がされなくなる可能性があります (???)。

GHC Haskell はより大きな賭けに出て、非同期例外を導入しています。これによってとてもエレガントな並行なコード (???) を簡単に書くことができるようになります。しかし同時に、不正確な例外処理の潜在的な可能性を大きく増大させることにもなります。

この記事では、基本的な部分から順にカバーしていきます:

* 例外の異なる型を定義する
* 同期例外処理を手直しする
* ボトムの値がどう登場するか
* 非同期例外の基礎
* マスキングと割り込みができないマスキング
* ヘルパーライブラリ
* もっと複雑な例

非同期例外を完璧に説明するためには、非同期例外そのものに特に関係ないトピックにまで踏み込んでカバーする必要があります。最初に非同期例外に全く関係がないように思えても、後でちゃんと説明するので驚かないでください。

私がみなさんに覚えて置いてほしいことは2つあります:

* ほとんどの場合、適切なヘルパーライブラリを使うことでうまくいきます。今日議論することの詳細まで全て覚えておく必要はないでしょう。理解することには意味はありますが。
* 今回は実行時の同期・非同期例外が GHC Haskell に含まれているものとし、これらを使うことが最善であるものとします (???)。本当に良いアイディアなのかどうか、いつ使うべきでいつ使うべきではないのかについては多くの議論があります。ここでは意図的にそれらを無視することにします。

最初に食欲を刺激しておきましょう。この話を読み終わる頃には、私がこの関数を `badRace` と呼ぶ理由をいくつか答えることができるようになっているはずです:

```haskell
badRace :: IO a -> IO b -> IO (Either a b)
badRace ioa iob = do
  mvar <- newEmptyMVar
  tida <- forkIO $ ioa >>= putMVar mvar . Left
  tidb <- forkIO $ iob >>= putMVar mvar . Right
  res <- takeMVar mvar
  killThread tida
  killThread tidb
  return res
```
