---
title: Haskell で非同期例外処理を扱う
author: Michael Snoyman
translator: pythonissam
tags: fpcomplete, 翻訳
---

Original post: [Asynchronous Exception Handling in Haskell](https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell)

![Exception Handling Text Small](https://www.fpcomplete.com/hubfs/Blog/Exception%20Handling%20Text%20Small.jpg?t=1526342142799)

先週、私は Haskell で非同期例外処理をする、というタイトルでウェブ講義をやりました。まだ見ていないのなら、[この動画](https://www.fpcomplete.com/blog/how-to-handle-asynchronous-exceptions-in-haskell)を見ることをおすすめします。[スライドも公開しています](https://www.snoyman.com/reveal/async-exception-handling)。

私にとって[習慣になりつつある](https://www.snoyman.com/blog/2017/12/what-makes-haskell-unique)のですが、この話のためにスライドを作る前に、ブログ記事を書く要領で内容を構成しました。テキストベースの学習が好きな人のために、その記事をここに置いておきます。

<!--more-->

実行時の例外は今日の多くのプログラミング言語でありふれたものです。これは諸刃の剣です。全ての関数の返り値をチェックする必要がなくなり、正しいコードが書きやすくなることはあります (たぶん)。一方で、コードがどこで処理を抜けたのか隠してしまい、リソースの解放がされなくなる可能性があります。

GHC Haskell はより大きな賭けに出て、非同期例外を導入しています。これによってとてもエレガントな並行なコードを簡単に書くことができるようになります。しかし同時に、不正確な例外処理の潜在的な可能性を大きく増大させることにもなります。

この記事では、基本的な部分から順にカバーしていきます:

* 例外の異なる型を定義する
* 正確な同期例外処理
* ボトムの値がどう登場するか
* 非同期例外の基礎
* マスキングと割り込みができないマスキング
* ヘルパーライブラリ
* もっと複雑な例

非同期例外を完璧に説明するためには、非同期例外そのものに特に関係ないトピックにまで踏み込んでカバーする必要があります。最初に非同期例外に全く関係がないように思えても、後でちゃんと説明するので驚かないでください。

私がみなさんに覚えて置いてほしいことは2つあります:

* ほとんどの場合、適切なヘルパーライブラリを使うことでうまくいきます。今日議論することの詳細まで全て覚えておく必要はないでしょう。理解することには意味はありますが。
* 今回は実行時の同期・非同期例外が GHC Haskell に含まれているものとし、これらを使うことが最善であるものとします。本当に良いアイディアなのかどうか、いつ使うべきでいつ使うべきではないのかについては多くの議論があります。ここでは意図的にそれらを無視することにします。

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

# 取り組む理由としての一例
例外の一番複雑な部分は、リソース不足や失敗の可能性があるアロケーションです。良い例がファイルの処理です。それには以下の処理が必要です:

* ファイルハンドルを開く。これは失敗しうる
* ファイルハンドルを扱う。これも失敗しうる
* ファイルディスクリプタは枯渇しがちなので、ファイルハンドルを注意して閉じる

## 純粋なコード
例外は純粋なコードで受け取ることはできません。これは設計上こうなっているもので、このトピックで扱うのが最適でしょう。正しい例外処理はリソースのアロケーションとその解放です。純粋なコードは不足しがちなリソースを確保することはできませんし、それを解放することもできません。そのため、例外を処理することはできません。

ただ、他の全てのルールのように、これにも例外はあります:

* 純粋なコードから投げることはできる
* アロケーションに `unsafePerformIO` を使うことができる
* 純粋なコードから、非明示的にメモリを確保することができる
  * 矛盾ではない! メモリを不足しがちなリソースとはみなさないので
* 本当に必要なら、またも `unsafePerformIO` 経由で例外を捕らえることができる。

しかし大部分で、私たちは純粋ではないコード、特に IO モナドに集中することにします。後で脱線して、トランスフォーマーにも言及することにします。

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

# 同期例外の国
さて、同期例外ができる Haskell の変種を使ってみましょう。例外のヒエラルキー辺りの話は後でするので、全ての例外が `IOException` だと仮定して話を進めましょう。2つのプリミティブ関数を導入します:

```haskell
throwIO :: IOException -> IO a
try :: IO a -> IO (Either IOException a)
```

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

* 型シグネチャを見て、`openFile` や `hClose` が失敗するのかどうか判別できなくなった
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

**通常の原則** アロケーションをするだけ、解放をするだけの関数の使用をできる限り避ける。その代わりに、どちらの操作も保証するヘルパー関数を使う。

しかし、`withFile` でさえもアロケーションと解放のどちらのアクションもするような何かに一般化することができます。これを `bracket` と呼ぶことにします。そして同期のみの世界では、こんな感じになるでしょう:

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

`Exception` 型クラスは、値を `SomeException` に変換する方法を定義しています。そして `SomeException` から与えられた型に変換する方法も定義しています。`throwIO` はその型クラスのインスタンスである任意の型について処理できるように一般化されています。`Show` インスタンスは例外を表示するためのもので、`Typeable` は実行時キャスティングのためのものです。

例外のデータ型の簡単な例です:

```haskell
data InvalidInput = InvalidInput String
  deriving (Show, Typeable)
instance Exception InvalidInput where
  toException ii = SomeException ii
  fromException (SomeException e) = cast e -- Typeable にある
```

しかし、`toException` と `fromException` のどちらも上のコードと同じようなデフォルト実装を持っているので、ただこのように書くことができます:

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

オブジェクト指向の用語では、`SubException` は `MyAppException` の子クラスである、と表現します。オブジェクト指向が採用されているのは気に食わないかもしれませんが、これは GHC Haskell の例外のメカニズムの一部です。そして、これは後々非同期例外を扱う上で、極めて重要になります。ここで議論しているのはそういう訳です。

さて、別の話題に進みましょう!

# 純粋なコードの例外
例外を投げる関数が `throwIO` というのもばからしいですね。なぜ単に `throw` じゃだめなんでしょうか? それは、不幸にもその名前が別のものに使われているからです:

```haskell
throw :: Exception e => e -> a
```

これは純粋なコードから例外を生成します。これらの種類の例外は、非同期例外と誤った呼び方をされます。最もふさわしくない名前なのに! このセクションは、この誤解を解くためのものです。これらの種類の例外は非純粋例外と呼ぶことにしましょう。なぜなら、純粋なコードを壊すからです。

これらの例外はいくつかの方法で生成することができます:

* `throw` 関数を直接使う
* `error` のような、`throw` 関数を呼んでいる関数を使う
* `head` のような部分関数を使う
* 不完全なパターンマッチ (GHC が自動的に `throw` の呼び出しと等しいものを呼ぶ)
* 純粋なコードで無限ループを作る。これで、GHC のランタイムが無限ループを検知して、実行時例外を投げるかもしれない

全体として、Haskell の世界では、部分性と非純粋例外は、冷ややかに見られます。なぜなら、根本的には嘘を言っているからです。ある値が `MyType` という型を持つことが主張されていても、実際には例外が待ち伏せているかもしれないわけです。しかしこの記事は審判を下すためのものではなく、物事に対処するために書いたものです。

非純粋例外を直接捉えるメカニズムはありません。`try` のような `IO` ベースの関数だけが、それを捉えることができます。例を見てみましょう:

```haskell
import Control.Exception
import Data.Typeable

data Dummy = Dummy
  deriving (Show, Typeable)
instance Exception Dummy

printer :: IO (Either Dummy ()) -> IO ()
printer x = x >>= print

main :: IO ()
main = do
  printer $ try $ throwIO Dummy
  printer $ try $ throw Dummy
  printer $ try $ evaluate $ throw Dummy
  printer $ try $ return $! throw Dummy
  printer $ try $ return $ throw Dummy
```

**質問** このプログラムの出力はどうなると思いますか?

この問題のキモは GHC の評価方法を知っているかどうかです。もしもそこまで詳しくない人だったら、答えを見て少し驚くかもしれません。興味を持つ方がいれば、評価に関する別の FPCO のウェビナーを作ることもできます。これが出力です:

```plain
Left Dummy
Left Dummy
Left Dummy
Left Dummy
Right Main.hs: Dummy
```

1. `throwIO Dummy` では、`throwIO` を使って正しく実行時例外を投げています。そして `Dummy` は実行時例外として即座に投げられます。そして `try` はそれを捉えることができ、全てがうまくいきます。

2. `throw Dummy` では、`IO ()` という型を持つ値を生成し、それが評価されると、`Dummy` という値を投げます。`try` にこの値を投げると、すぐに評価し、実行時例外が投げられることになります。結果は `throwIO` のときと同じです。

3. `evaluate $ throw Dummy` では、`throw Dummy` は `()` という型を持つことになります。`evaluate` 関数はその値の評価を強制するので、`Dummy` という例外が投げられます。

4. `return $! throw Dummy` はほとんど今までのものと同じですが、`$!` を使っていて、これは評価を強制するために、水面下で `seq` を使っています。今日は `evaluate` と `seq` の違いに飛び込むのはやめておきましょう。

5. `return $ throw Dummy` は異質です。`()` 型の `throw Dummy` を使ってサンクを作っていて、それが評価されれば例外が投げられるはずです。それは `return` を使って `IO ()` の値にラップされています。`try` は `IO ()` の値の評価を強制していますが、`()` の値の評価までは強制しません。そのため、まだ実行時例外が投げられることはありません。次に `Either Dummy ()` という型の値が出てきます。これは `Right (throw Dummy)` という値です。`printer` はこの値を出力しようとし、最終的に `throw Dummy` を評価します。 この値は `try` によって処理されていないので、プログラムはクラッシュします。

いいでしょう、では、以上のポイントは何だったんでしょうか? 2つあります:

1. 今回、何かについて判決を下すことはありませんでしたが、少しそれをやってみましょう。非純粋例外は本当に困惑させられるものです。`throw` と `error` を使うのは、部分関数と不完全なパターンマッチと同様、できれば常に避けるべきです。例外を扱うときには、`throwIO` を使ってください。

2. 例外の値がほとんどランダムな位置に現れるように見えても、あなたのプログラムをぶち壊す、非純粋例外のトリガーは常に同じです。例外を隠しているサンクを評価することです。なので、非純粋例外は絶対に同期例外です。今現在扱っている `IO` アクションが、例外を投げる原因になります。

ほとんどの場合、私たちは例外安全なコードを書く上で、非純粋例外について深く考えることはないでしょう。`withFile` の例をもう一度見てみましょう:

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = do
  handle <- openFile fp mode
  eres <- try (inner handle)
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

もしも `inner` が非純粋例外を返しても、`withFile` の中では別に問題にはなりません。返ってきた値を評価することがないからです。今回は、非純粋例外についてほとんど無視することにします。そして同期例外と非同期例外に集中することにします。

# 非同期例外のメリット
そもそも、なぜ非同期例外なんてものが必要とされたのか考えてみましょう。簡単な例から始めます。`timeout` 関数です。アクションを決められた時間だけ走らせて、それまでに終わらなかったらそれを殺すような関数が欲しいとします:

```haskell
timeout :: Int -- microseconds
        -> IO a -> IO (Maybe a)
```

そして、これを直接ランタイムシステムに組み込み、すぐにスレッドが死ぬことを許可したとします。以下のようなプログラムを作りました:

```haskell
timeout 1000000 $ bracket
  (openFile "foo.txt" ReadMode)
  hClose
  somethingReallySlow
```

`somethingReallySlow` は1秒以内で処理が終わりました。しかし1秒以上かかったらどうでしょうか? 既に言及したように、スレッドは単純にすぐに死にます。結果 `hClose` が実行中に呼ばれることはありません。これは例外安全の仕組みをぶち壊しています!

その代わりに、ランタイムシステムの外側で何か作ってみましょう。タイムアウトになったかどうか、追跡するミュータブル変数を作り、操作の結果を `MVar` に保存します。そして、ヘルパー関数を作って、スレッドを終了させるべきかどうか確認することにします。こんな感じになるでしょうね:

```haskell
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (when, forever)
import Data.IORef
import Data.Typeable

data Timeout = Timeout
  deriving (Show, Typeable)
instance Exception Timeout

type CheckTimeout = IO ()

timeout :: Int -> (CheckTimeout -> IO a) -> IO (Maybe a)
timeout micros inner = do
  retval <- newEmptyMVar
  expired <- newIORef False
  let checkTimeout = do
        expired' <- readIORef expired
        when expired' $ throwIO Timeout
  _ <- forkIO $ do
    threadDelay micros
    writeIORef expired True
  _ <- forkIO $ do
    eres <- try $ inner checkTimeout
    putMVar retval $
      case eres of
        Left Timeout -> Nothing
        Right a -> Just a
  takeMVar retval

myInner :: CheckTimeout -> IO ()
myInner checkTimeout = bracket_
  (putStrLn "allocate")
  (putStrLn "cleanup")
  (forever $ do
    putStrLn "In myInner"
    checkTimeout
    threadDelay 100000)

main :: IO ()
main = timeout 1000000 myInner >>= print
```

光の面としては、この実装はもともと存在していた実行時例外のシステムを再利用して、例外安全を保証してくれています。やったね! しかし、このアプローチのダークサイドを分析してみましょう:

* `checkTimeout` は `IO` の中で動くので、純粋なコードの中で実行することができない。 つまり、これは CPU を長く使う計算を中断させることができないということを意味している。
* 関連する全ての場所で、`checkTimeout` を呼ぶ必要がある。そうしないと、`timeout` は正確に動かない。

**ボーナス問題** 上のコードは、誤った同期例外の扱い方をしていて、デッドロックを起こす可能性があります。見つけてみてください!

この種類のアプローチは動くことには動きますが、書いていて楽しいものではありません。非同期例外に進んでみましょう。

# 非同期例外
**非同期例外とは、別のスレッドから投げられる例外です**。例外が発生する現在実行中のスレッドでは、何も実行されません。この例外は、同期例外と同じように発生します。`try` (と `catch` の類) で例外を捉えることができる点も同じです。違いは、どのように投げられるかということだけです:

```haskell
forkIO :: IO () -> IO ThreadId
throwTo :: Exception e => ThreadId -> e -> IO ()
```

上の 自家製 `timeout` の例では、`throwTo` の呼び出しは `expired` を `True` にセットすることと対応しています。問題は、対象のスレッドがいつ、`expired` が `True` にセットされたか / 非同期例外が投げられたか確認するのか? ということです。答えは、ランタイムシステムが勝手にやってくれる、です。そしてこれが重要な部分なのですが、**ランタイムシステムはいつでも非同期例外を検知することができます**。これには、純粋なコードの中の例外も含まれます。これは、自家製の `timeout` の2つの問題点を解決してくれますが (??? checkTimeout は IO の中で動くのでということとどう対応しているの???) 、また新しい問題を作ることにもなります。

## マスキングの必要性
もう一度 `withFile` 関数を見てみましょう:

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = do
  handle <- openFile fp mode
  eres <- try (inner handle)
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

今は、ランタイムシステムがやってくれている、非同期の確認アクションを入れておきましょう:

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = do
  checkAsync -- 1
  handle <- openFile fp mode
  checkAsync -- 2
  eres <- try (inner handle)
  checkAsync -- 3
  hClose handle
  checkAsync -- 4
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

`checkAsync` の (1) か (4) が例外を投げたのなら、何の問題もありません。 しかし (2) か (3) が投げたのなら、リソースがリークするか、`hClose` が呼ばれなくなります! よって、ランタイムシステムに、「今は非同期例外を確認しないでくれ」と伝える手段が必要になります。これをマスキングと呼びます。そして、これをデモするために、`mask_` という関数を導入します:

```haskell
mask_ :: IO a -> IO a
```

この関数は、「与えられたアクションを実行してくれ。そして、これを実行している間は非同期例外を検出しないでくれ」ということを言っています。こいつを使って、`withFile` を手直しします:

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = mask_ $ do
  -- doesn't run, masked! -- checkAsync -- 1
  handle <- openFile fp mode
  -- same -- checkAsync -- 2
  eres <- try (inner handle)
  -- same -- checkAsync -- 3
  hClose handle
  -- same -- checkAsync -- 4
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

さて、リソースリークは解決しましたが、また新たな問題が生まれました。非同期例外を `inner` も含め、`withFile` の内側のどこかに送る方法が完全に失われました。このままでは、ユーザが提供するアクションの実行に長い時間がかかれば、`timeout` 関数の存在意義が無くなってしまいます。これをどうにかするために、`mask` 関数を使う必要があって、これは前回のマスキングの状態を復元する方法を提供してくれます:

```haskell
mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
```

**発展** この関数がなぜ、マスキングを解除するのではなく、前回のマスキングの状態を復元するのかと不思議に思うかもしれません。これはネストしたマスキングと関係があり、「ワームホール」問題と言います。これについて深く掘り下げることはしません。

これでかなりいい感じの `withFile` を書くことができます:

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = mask $ \restore -> do
  handle <- openFile fp mode
  eres <- try (restore (inner handle))
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

マスキングの状態を上のコードの位置で復元するのは完全に安全です。なぜなら、`try` でラップされているので、全ての非同期例外を捉えることができるからです。そのため、何があっても `openFile` が成功したときには、`hClose` が呼ばれることは保証されています。

## みーんなゲットだぜ!
型チェックをさせるために、`withFile` の例をもう一捻りする必要があります。該当する部分を見てみましょう:

```haskell
eres <- try (restore (inner handle))
case eres of
  Left e -> throwIO e
```

ここでの問題は、`try` と `throwIO` が、例外の型について多相的であることです (`Exception` のインスタンスであれば何でもよい)。GHC は、あなたが具体的にどんな型を求めているのか知らされていません。今回は、*全ての*例外を捉えることにします。そのためには、`SomeException` 型、オブジェクト指向の世界では全例外クラスのスーパークラスのようなものを使います。必要なものは型シグネチャだけです:

```haskell
eres <- try (restore (inner handle))
case eres of
  Left e -> throwIO (e :: SomeException)
```

## 復帰 vs 解放
今までのコードには何も間違っている部分はありませんでした。今度は少し違うコードを使って、問題が起こるかどうか見てみましょう。

```haskell
import Control.Concurrent
import Control.Exception
import Data.Time
import System.Timeout

main :: IO ()
main = do
  start <- getCurrentTime
  res <- timeout 1000000 $ do
    x <- try $ threadDelay 2000000
    threadDelay 2000000
    return x
  end <- getCurrentTime
  putStrLn $ "Duration: " ++ show (diffUTCTime end start)
  putStrLn $ "Res: " ++ show (res :: Maybe (Either SomeException ()))
```

このプログラムの出力は:

```plain
Duration: 3.004385s
Res: Just (Left <<timeout>>)
```

タイムアウトは起こりましたが、

* 実行時間は3秒で、1秒ではなかった
* 結果は `Nothing` ではなく、`Just` だった
* `Just` の中身はタイムアウトからの例外だった

私たちは非同期例外を捉えるために、全ての例外を捉えることにしました。`withFile` の例ではうまくいくと言いましたが、いくつかの理由から、ここではうまくいかなくなります。この状況を支配しているルールは簡単です:

**非同期例外から元の状態に復帰することはできない**

適切な非同期例外処理、という文脈で、このルールはよく抽象的に語られます。シンプルなアイディアで、実際に説明することも、そして実装することも難しくはありません。しかし、この「安全な非同期例外処理」という言葉の持つ本質的な抽象性には、本来あるべきではない恐ろしさがあります。具体的に考えてみましょう。


例外を捉える動機は2つあります:

* 例外が起きる前に、解放処理を走らせる必要があるとき。これは `withFile` の例でやったことです。例外を捉え、解放処理を走らせ、例外を投げ直しました。
* アクションが例外を投げ、それが捉えてスレッド全体を殺すのではなく、元の状態に復帰したいとき。例えば、ファイルを読もうとして、それが存在しなかったとき、デフォルトの値を使いたいときです。この場合、例外を捉えてそれを投げ直すのではなく、**捉えた後に捨てることになります**。

同期例外を扱うときは、リソースを解放してその例外を投げ直しても、例外を捉えて、それを捨てた上で例外から復帰しても問題はありません。その世界の不変性を壊してはいないからです。

しかし、非同期例外では、復帰したいと思うことはないでしょう。非同期例外は、現在実行している世界の外側から送る、「お前は今すぐ死ななければならない」というメッセージのようなものです。こういった例外を捨ててしまうと、`timeout` の例でやったように、非同期例外のメカニズムの本質的な部分を壊すことになります。そのため、非同期例外では復帰ではなく、リソースの解放をするわけです。

さて、理論はいいでしょう。しかし、実際にはどうやって実装しているのでしょうか?

## GHC の非同期例外処理の欠陥
例外を発生させるとき、どうやって例外が同期例外か、非同期例外か判別すればよいのでしょうか? それは簡単です。最終的に `throwIO` 関数 (同期) を使っているか、`throwTo` 関数 (非同期) を使っているかどうかです。そのため、このロジックを実装するためには、`try` を使った後に、どちらの関数が例外を投げたのか確認する方法が必要になってきます。

しかし残念なことに、そのような関数は存在しません。それはライブラリ関数が存在しないというだけの問題ではなく、GHC のランタイムシステムそのものが、例外に関する情報を何一つ追跡しないのです。よって、この方法で区別することはできません!

私は長年、同期・非同期例外を区別するために、2つの方法を取ってきました。昔使っていた方法はスレッドをフォークするというもので、`enclosed-exceptions` というパッケージに取りこまれています。ただ、これは重い処理なので、今使うのはおすすめしません。それに代わって最近は、型ベースのアプローチをおすすめするようにしています。これは `safe-exceptions` と `unliftio` パッケージに取りこまれています。(この3つのパッケージについては後ほど説明します。)

### 警告という言葉
今から解説しようとしているメカニズムは、直接 `Control.Exception` を使うことでだますことは十分可能です。そのため、総合的にこのモジュールを直接使うのは避け、私が今から説明する、型ベースのロジックを実装しているヘルパーモジュールを使用することをおすすめします。故意に型ベースの検知を使わなかった場合、この後議論する不変性が壊れて終わります。ここでは `Control.Exception` を使うほとんどの場合において、このメカニズムを壊すことがその目的にある、と述べておきます。

GHC が あの笑える オブジェクト指向っぽい例外の階層を実現する上で、どのように拡張可能な例外のメカニズムを実装していたのか、思い出してください。全ての例外が、どのように `SomeException` の子クラスになっていたのか思い出してください。GHC 7.8 から、`SomeException` には新しい子クラスが追加されました。`SomeAsyncException` です。これは全ての非同期例外型のスーパークラスになっています。これで、例外が非同期例外型かどうか、以下のような関数で確認することが可能になりました:

```haskell
isSyncException :: Exception e => e -> Bool
isSyncException e =
    case fromException (toException e) of
        Just (SomeAsyncException _) -> False
        Nothing -> True

isAsyncException :: Exception e => e -> Bool
isAsyncException = not . isSyncException
```

`throwIO` と `throwTo` はそれぞれ、同期例外、非同期例外でのみ使われるということを保証したいですよね。これは、ヘルパー型、ラッパー型を使って解決することにします:

```haskell
data SyncExceptionWrapper = forall e. Exception e => SyncExceptionWrapper e
instance Exception SyncExceptionWrapper

data AsyncExceptionWrapper = forall e. Exception e => AsyncExceptionWrapper e
instance Exception AsyncExceptionWrapper where
    toException = toException . SomeAsyncException
    fromException se = do
        SomeAsyncException e <- fromException se
        cast e
```

次に変換用のヘルパー関数です:

```haskell
toSyncException :: Exception e => e -> SomeException
toSyncException e =
    case fromException se of
        Just (SomeAsyncException _) -> toException (SyncExceptionWrapper e)
        Nothing -> se
  where
    se = toException e

toAsyncException :: Exception e => e -> SomeException
toAsyncException e =
    case fromException se of
        Just (SomeAsyncException _) -> se
        Nothing -> toException (AsyncExceptionWrapper e)
  where
    se = toException e
```

それから、`throwIO` と `throwTO`, `impureThrow` (`throw` 関数の代替) をいじって実装します:

```haskell
import qualified Control.Exception as EUnsafe

throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . EUnsafe.throwIO . toSyncException

throwTo :: (Exception e, MonadIO m) => ThreadId -> e -> m ()
throwTo tid = liftIO . EUnsafe.throwTo tid . toAsyncException

impureThrow :: Exception e => e -> a
impureThrow = EUnsafe.throw . toSyncException
```

全ての例外はこれら3つの関数から生成されるとして、これで例外の種類の区別を型に頼ることができます。最後のステップとして、ヘルパー関数を任意の例外型について動く **cleanup** (して例外を投げ直す) 系関数と、**recover** (して例外を投げ直さない) 系関数に分けることにします。不完全ですが、こういった分け方になります:

* 復帰
  * `catch`
  * `try`
  * `handle`
* 解放
  * `bracket`
  * `onException`
  * `finally`

`catch` 関数を簡潔に書くとこんな感じになります:

```haskell
import qualified Control.Exception as EUnsafe

catch :: Exception e => IO a -> (e -> IO a) -> IO a
catch f g = f `EUnsafe.catch` \e ->
  if isSyncException e
    then g e
    -- 意図的に非同期例外を同期的に投げ直している
    -- これは非同期例外の振る舞いを維持するため
    else EUnsafe.throwIO e
```

これらのヘルパー関数を使えば、安全な非同期例外処理のルールは自動的に満たされることになります。ポケモンのような例外ハンドラを作ることも可能です ([ポケモンみーんなゲットだぜ](http://eikaiwa.dmm.com/blog/40042/)):

```haskell
tryAny :: MonadUnliftIO m => m a -> m (Either SomeException a)
tryAny = try

main :: IO ()
main = tryAny (readFile "foo.txt") >>= print
```

## 割り込み不可能なマスキング
このウサギの巣 (訳者注: 不思議の国のアリスから派生して、不思議なものという意味) に飛び込む前に、思い出してください。`Control.Exception.Safe` や `UnliftIO.Exception` を使えば、多くの場合において、割り込み可能なマスキングとそうではないマスキングを、正しく扱ってくれます。もうこれを心配することはありません。コーナーケースのバグも多くありますが、経験上、例外安全なコードを書くときにそのバグが起こることはあまりありません。

さて、これまで同期例外 (今現在のスレッドのアクションによって生成された例外) と非同期例外 (別のスレッドで生成され、今現在のスレッドに投げられた例外) の2つの例外を説明してきました。そして、一時的に全ての非同期例外をブロックする `mask` 関数を紹介しましたね?

...という説明は厳密には正しくありません。GHC のドキュメントによると、

> いくつかの操作は割り込み可能です。つまり、mask のスコープの内側でも、非同期例外を受け取ることができます。自分自身をブロックするかもしれない関数は全て割り込み可能です。`mask` は、非同期例外を完全にブロックする関数だと考えるのではなく、非同期モードからポーリングモードに切り替える関数だと考えるのがよいでしょう。

割り込み可能な操作は、デッドロックを防ぐ手段になります。これもドキュメントから拝借したものですが、以下の例を考えてみましょう:

```haskell
mask $ \restore -> do
  a <- takeMVar m
  restore (...) `catch` \e -> ...
```

もしも `takeMVar` が割り込み不可能だったなら、値がセットされることがない `MVar` をブロックすることができず、デッドロックする可能性があります。その代わりに、GHC のランタイムは、`mask` されたセクション内でも、いくつかのアクションはポールすることができ、待機している非同期例外があるかどうか確認することができる、といったコンセプトを導入しています。

しかし残念なことに、こうすることで、私たちが当初 `mask` を導入した目的に背く結果になることがあります。また、リソースの解放が常に起こることを保証できなくなります。そのため、割り込み可能なアクションの中でも、非同期例外をブロックする別の関数が用意されています。`uninterruptibleMask` です。GitHub でも議論されているように、それぞれの関数をどんな場合に使うか、絶対的な線引きがあるわけではありません。一般的なルールはこうです:

* `mask` の中にいるのなら、いつでもその中で `uninterruptibleMask` に格上げすることができる。マスクされていない状態からマスクされている状態にすることはできない。なぜなら、マスクされていないコードでは、割り込み可能なアクションの中だけではなく、非同期例外はどこでも発生することができるから。

* 可能なときは常に、どんなマスキング関数であっても使うべきではない。これらの関数は複雑で、低レベルな関数である。その代わりに、`bracket` や `finally` 等の高レベルの関数を使うこと。

* `uninterruptibleMask` は、完全なデッドロックを引き起こす可能性がある。割り込み可能な `mask` は、解放処理をするアクションを中断させてしまう可能性、そして解放処理をするアクション以前のアクションを中断させ、解放処理をするアクションの呼び出しを妨げる可能性がある。どうしてもマスキング関数を直接使いたいのなら、その目的をよく考える必要がある。

# ヘルパーライブラリ関数
さっきより安全な非同期例外処理をするために推奨される、3つのヘルパーライブラリについて言及しました。もう少し掘り下げてみましょう:

* `enclosed-exceptions` は非同期例外を特定するために、フォークしたスレッドを使った古いアプローチを取っています。新しくコードを書くときにこの方法はおすすめしません。
* 他の2つのライブラリは、どちらもこの記事で解説した型ベースの判別手法を採用しています。違いは、モナドトランスフォーマの扱い方です:
  * `safe-exceptions` は、`MonadCatch` や `MonadMask` といった `exceptions` 由来の型クラスを使っている
  * それに対し、`unliftio` は `MonadUnliftIO` を使っている

適切なモナドトランスフォーマの扱い方は、完全に別のトピックで、別の場所で解説しています (スライドやビデオとか)。私なら、新しく書く全てのコードで `unliftio` を使うことをおすすめします。

# 安全な非同期例外処理のルール
Haskell で適切な例外安全なコードを書くための、今まで出てきたルールをまとめてみましょう。

* 何らかの解放処理など、もしも何かをする必要があるなら、`mask` や `uninterruptibleMask` を使って、一時的に非同期例外を切らなければならない
* 非同期例外を一度でも捉えたら、投げ直さなければならない (復帰は許されない)
* 非同期例外にすばやく反応するために、マスクされている時間をできるだけ小さくするべき
  * これをさらに拡張して、解放処理にかける時間も小さくするべき。例えば一例として、解放処理のコードの中で複雑なネットワークプロトコルを走らせたりするのは良いアイディアではない

しかし、正しいライブラリとライブラリ関数を使うことで、その都度頭を悩ませることなく、以上のルールを正しく守ることができるでしょう。

# 例
さて、これで Haskell での例外処理の全ての原則をカバーしたことになります。ベストプラクティスを紹介するためにも、これからいくつかの例をお見せしましょう。

## 可能な限り非同期例外を避ける
これは一般的なアドバイスなのですが、必要ないときにっは非同期例外を使わないでください。時に、非同期例外はメッセージパッシングやフローのコントロールに使われたりします。そういうことをしたいのなら、多くの場合もっと良い方法があります! 以下のコードを考えてみましょう:

```haskell
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

main :: IO ()
main = do
  messages <- newChan
  race_
    (mapM_ (writeChan messages) [1..10 :: Int])
    (forever $ do
      readChan messages >>= print
      -- simulate some I/O latency
      threadDelay 100000)
```

これはメッセージを床にぶちまけるだけで終わります。なぜなら、最初のスレッドは2番目のスレッドが終了する前に終わるからです。`forever` を使って非同期例外に殺しを頼むのではなく、チャンネルそのものに組み込んでしまいましょう:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-11.4 script --package unliftio --package stm-chans
import UnliftIO (concurrently_, atomically, finally)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TBMQueue
import Data.Function (fix)

main :: IO ()
main = do
  messages <- newTBMQueueIO 5
  concurrently_
    (mapM_ (atomically . writeTBMQueue messages) [1..10 :: Int]
     `finally` atomically (closeTBMQueue messages))
    (fix $ \loop -> do
      mmsg <- atomically $ readTBMQueue messages
      case mmsg of
        Nothing -> return ()
        Just msg -> do
          print msg
          -- simulate some I/O latency
          threadDelay 100000
          loop)
```

ここから得られる教訓は、非同期例外は強力で、多くのコードを簡単に正しく書くことができるが、必要ではなかったり、役に立たないこともままある、ということですね。
