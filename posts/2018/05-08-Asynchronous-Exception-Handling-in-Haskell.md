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

オブジェクト指向の用語では、`SubException` は `MyAppException` の子クラスである、と表現します。オブジェクト指向が採用されているのは気に食わないかもしれませんが、これは GHC Haskell の例外のメカニズムの一部です。そして、これは後々非同期例外を扱う上で、極めて重要になります。ここで議論しているのはそういう訳です。

さて、別の話題に進みましょう!

# 純粋なコードの例外
例外を投げる関数が `throwIO` というのもばからしいですね。なぜ単に `throw` じゃだめなんでしょうか? それは、不幸にもその名前が別のものに使われているからです:

```haskell
throw :: Exception e => e -> a
```

これは純粋なコードから例外を生成します。これらの種類の例外は、同期例外と誤った呼び方をされます。最もふさわしくない名前なのに! このセクションでは、この誤解を解くためのものです。これらの種類の例外は非純粋例外と呼ぶことにしましょう。なぜなら、純粋なコードを壊すからです。

これらの例外はいくつかの方法で生成することができます:

* `throw` 関数を直接使う
* `error` のような、`throw` 関数を呼んでいる関数を使う
* `head` のような部分関数を使う
* 不完全なパターンマッチ (GHC が自動的に `throw` の呼び出しと等しいものを呼ぶ)
* 純粋なコードで無限リストを作る。これで、GHC の実行時システム (???) が無限ループを検知して、実行時例外を投げるかもしれない

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

1. `throwIO Dummy` では、`throwIO` を使って正しく実行時例外を投げています。そして `Dummy` は実行時例外として即座に投げられます。そして `try` はそれを捉えることができ、全てがうまくいきます。

2. `throw Dummy` では、`IO ()` という型を持つ値を生成し、それが評価されると、`Dummy` という値を投げます。`try` にこの値を投げると、すぐに評価し、実行時例外が投げられることになります。結果は `throwIO` のときと同じです。

3. `evaluate $ throw Dummy` では、`throw Dummy` は `()` という型を持つことになります。`evaluate` 関数はその値の評価を強制するので、`Dummy` という例外が投げられます。

4. `return $! throw Dummy` はほとんど5番と同じですが、`$!` を使っていて、これは水面下で `seq` を使っています。今日は `evaluate` と `seq` の違いに飛び込むのはやめておきましょう。

5. `return $ throw Dummy` は異質です。`()` 型の `throw Dummy` を使ってサンクを作っていて、それが評価されれば例外が投げられるはずです。それは `return` を使って `IO ()` の値にラップされています。`try` は `IO ()` の値の評価を強制していますが、`()` の値の評価までは強制しません。そのため、まだ実行時例外が投げられることはありません。次に `Either Dummy ()` という型の値が出てきます。これは `Right (throw Dummy)` という値です。`printer` はこの値を出力しようとし、最終的に `throw Dummy` を評価します。 この値は `try` によって処理されていないので、プログラムはクラッシュします。

いいでしょう、では、以上のポイントは何だったんでしょうか? 2つあります:

1. 今回、何かについて判決を下すことはありませんでしたが、少しやってみましょう。非純粋例外は本当に困惑させられるものです。`throw` と `error` を使うのは、部分関数と不完全なパターンマッチと同様、できれば常に避けるべきです。例外を扱うときには、`throwIO` を使ってください。

2. 例外の値がほとんどランダムな位置に現れるように見えても、あなたのプログラムをぶち壊す、非純粋例外のトリガーは常に同じです。例外を隠しているサンクを評価することです。なので、非純粋例外は絶対に同期例外です (???)。今現在扱っている `IO` アクションが、例外を投げることになります。

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

# 同期例外のメリット
そもそも、なぜ同期例外なんてものが必要とされたのか考えてみましょう。簡単な例から始めます。`timeout` 関数です。アクションを決められた時間だけ走らせて、それまでに終わらなかったらそれを殺すような関数が欲しいとします:

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

`somethingReallySlow` に対して、処理を完遂するのに1秒与えました。しかし1秒以上かかったらどうでしょうか? 既に言及したように、スレッドは単純にすぐに死にます。結果 `hClose` の処理が終わることはありません (??? hClose の処理が終わってなかったってこと?)。これは例外安全の仕組みをぶち壊しています!

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

光の面としては、この実装はもともと存在していた実行時例外のシステムを再利用して、例外安全を保証してくれています (???)。やったね! しかし、このアプローチのダークサイドを分析してみましょう:

* `checkTimeout` は `IO` の中で動くので、純粋なコードの中で使うことができない。 つまり、これは CPU を長く使う計算を中断させることができないということを意味している (???)。
* 関連する全ての場所で、`checkTimeout` を呼ぶ必要がある。そうしないと、`timeout` は正確に動かない。

**ボーナス問題** 上のコードは、誤った同期例外の扱い方をしていて、デッドロックを起こす可能性があります。見つけてみてください!

この種類のアプローチは動くことには動きますが、書いていて楽しいものではありません。非同期例外に進んでみましょう。
