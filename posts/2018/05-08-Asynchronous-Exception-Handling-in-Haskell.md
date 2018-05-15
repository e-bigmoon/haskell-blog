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

# モチベーションの例 (???)
例外の一番複雑な部分は、希少なリソース (???) や失敗の可能性があるアロケーションです。良い例がファイルの処理です。それには以下の処理が必要です:

* ファイルハンドルを開く。これは失敗しうる
* ファイルハンドルを扱う。これも失敗しうる
* ファイルディスクリプタは希少なリソースなので、ファイルハンドルを閉じる (???)

## 純粋なコード
例外は純粋なコードで受け取ることはできません。これは設計上こうなっているもので、このトピックで扱うのが最適でしょう。正しい例外処理はリソースのアロケーションとその後始末です (???)。純粋なコードは希少なリソースを確保することはできませんし、それを始末することもできません。そのため、例外を処理することはできません。

ただ、他の全てのルールのように、これにも例外はあります:

* 純粋なコードから投げることはできる
* アロケーションに `unsafePerformIO` を使うことができる
* 純粋なコードから、非明示的にメモリにアクセスすることができる
  * 矛盾ではない! メモリを希少なリソースとはみなさないので
* 本当に必要なら、またも `unsafePerformIO` 経由で例外を捕らえることができる。

しかし大部分で、私たちは純粋ではないコード、特に IO モナドに集中することにします。後で脱線して、トランスフォーマーにも言及することにします (???)。

# 例外が存在しない国
実行時例外が存在しない理論上の Haskell で、ファイルを扱ってみましょう。全ての失敗するケースを、具体的な返り値ごとに表現する必要があります。

```haskell
openFile :: FilePath -> IOMode -> IO (Either IOException Handle)
hClose :: Handle -> IO () -- こいつは失敗できないと考えて
usesFileHandle :: Handle -> IO (Either IOException MyResult)

myFunc :: FilePath -> IO (Either IOException MyResult)
myFunc fp = do
  ehandle <- openFile fp ReadMode
  case ehandle of
    Left e -> return (Left e)
    Right handle -> do
      eres <- usesFileHandle handle
      hClose handle
      return eres
```

型システムのせいで、それぞれの関数が成功したのか失敗したのか、具体的に確認することを強制されます。`usesFileHandle` のケースでは、本質的に失敗を無視して、それを関数の呼び出し元へ渡します。そして `hClose` が呼ばれることを保証しています。

# LAND OF SYNCHYRONOUS EXCEPTIONS
Now let's uses a variant of Haskell which has synchronous exceptions. We'll get into exception hierarchy stuff later, but for now we'll just assume that all exceptions are IOExceptions. We add in two new primitive functions:

# 同期例外の国
さて、同期例外ができる Haskell のバリアントを使ってみましょう。例外のヒエラルキー辺りの話は後でするので、全ての例外が `IOException` だと仮定して話を進めましょう。2つのプリミティブ関数を導入します:

```haskell
throwIO :: IOException -> IO a
try :: IO a -> IO (Either IOException a)
```

These functions throw synchronous exceptions. We'll define synchronous exceptions as:

**Synchronous exceptions are exceptions which are generated directly from the IO actions you are calling.**

Let's do the simplest transformation from our code above:

これらの関数は同期例外を投げます。ここでは同期例外を以下のように定義します:

**同期例外とは、呼んでいる `IO` アクションから直接生成される例外のことである。**

さっきのコードを一番シンプルにいじってみましょう:

```haskell
openFile :: FilePath -> IOMode -> IO Handle
hClose :: Handle -> IO ()
usesFileHandle :: Handle -> IO MyResult

myFunc :: FilePath -> IO MyResult
myFunc fp = do
  handle <- openFile fp ReadMode
  res <- usesFileHandle handle
  hClose handle
  return res
```

コードは確実に短くなっていて、型もより読みやすくなりました。変わったものとしては:

* 型シグネチャを見て、`openFile` や `hClose` が失敗するのかどうか判別できなくなった (???)
* `openFile` の結果をパターンマッチする必要がなくなった。これは自動的に処理される

不幸な話ですが、このコードにはバグがあります! もしも `usesFileHandle` が例外を投げたらどうなるでしょうか。`hClose` が呼ばれることはありません。これを `try` と `throwIO` を使って直せるかやってみましょう:

```haskell
myFunc :: FilePath -> IO MyResult
myFunc fp = do
  handle <- openFile fp ReadMode
  eres <- try (usesFileHandle handle)
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

私たちのコードは、少なくとも同期例外の世界において、例外に対して安全になりました。

しかし残念なことに、これはすごく良い方法というわけではありません。私たちは人民に対して、ファイルを扱う度に同じことを考えさせたくありません。そのため、ヘルパー関数でこのパターンを捉えることにします:

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = do
  handle <- openFile fp mode
  eres <- try (inner handle)
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res

myFunc :: FilePath -> IO MyResult
myFunc fp = withFile fp ReadMode usesFileHandle
```

**通常の原則** アロケーションをするだけ、掃除をするだけの関数の使用をできる限り避ける。その代わりに、どちらの操作も保証するヘルパー関数を使う。

しかし、`withFile` でさえもアロケーションと掃除のどちらのアクションもするような何かに一般化することができます。これを `bracket` と呼ぶことにします。そして同期のみの世界では、こんな感じになるでしょう:

```haskell
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket allocate cleanup inner = do
  a <- allocate
  ec <- try (inner a)
  _ignored <- cleanup a
  case ec of
    Left e -> throwIO e
    Right c -> return c

withFile fp mode = bracket (openFile fp mode) hClose
```

**質問** `cleanup` が例外を投げると何が起こるでしょうか? 何が起こるようにするべきでしょうか?

**解答** 無視される。が、この場合 `throwIO` で例外を投げるようにすべき。

# 拡張可能な例外
`catch` と `throwIO` に使った型シグネチャは、実際には嘘です。私たちは全ての例外が `IOException` の型を持つものとして話を進めてきました。が、実際には、GHC のおかげで任意の型を作って例外として投げることができます。背景にある考えは Java と同じで、クラスのヒエラルキーを作るようなものです。

関連する定義を見てみましょう:

```haskell
data SomeException = forall e . Exception e => SomeException e

class (Typeable e, Show e) => Exception e where
  toException   :: e -> SomeException
  fromException :: SomeException -> Maybe e

throwIO :: Exception e => e -> IO a
try :: Exception e => IO a -> IO (Either e a)
```

`Exception` 型クラスは、値を `SomeException` に変換する方法を定義しています。そして `SomeException` から与えられた型に変換する方法も定義しています。`throwIO` はその型クラスのインスタンスである任意の型について処理できるように一般化されています。`Show` インスタンスは例外を表示するためのもので、`Typeable` は実行時キャスティング (???) のためのものです。

例外のデータ型の簡単な例です:

```haskell
data InvalidInput = InvalidInput String
  deriving (Show, Typeable)
instance Exception InvalidInput where
  toException ii = SomeException ii
  fromException (SomeException e) = cast e -- Typeable にある
```

しかし、`toException` と `fromException` のどちらも上のコードと同じようなデフォルト実装を持っているので、(???) ただこのように書くことができます:

```haskell
instance Exception InvalidInput
```

例外のヒエラルキーを作ることもできます。例えば、

```haskell
{-# LANGUAGE ExistentialQuantification #-}
import Control.Exception
import Data.Typeable

data MyAppException
  = InvalidInput String
  | SomethingElse SomeException
  deriving (Show, Typeable)
instance Exception MyAppException

data SubException = NetworkFailure String
  deriving (Show, Typeable)
instance Exception SubException where
  toException = toException . SomethingElse . SomeException
  fromException se = do
    SomethingElse (SomeException e) <- fromException se
    cast e

main :: IO ()
main = do
  e <- try $ throwIO $ NetworkFailure "Hello there"
  print (e :: Either SomeException ())
```

オブジェクト指向の用語では、`SubException` は `MyAppException` の子クラスである、と表現します。オブジェクト指向が採用されているのは気に食わないかもしれませんが、これは GHC Haskell の例外のメカニズムです。そして、これは後々非同期例外を扱う上で、極めて重要になります。ここで議論しているのはそういう訳です。

さて、別の話題に進みましょう!
