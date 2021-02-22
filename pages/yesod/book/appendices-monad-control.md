---
title: 'monad-control'
published: 2021/02/21
---

monad-controlはYesodにおいていくつかの場所で用いられ, 主にPersistentにおける適切な例外処理を保証している. これは一般的にも用いられるパッケージであり, 
モナドトランスフォーマにおける標準的な機能を拡張する. 

## 概要

最も強力で, 時に混乱してしまうようなHaskellの特徴の1つはモナドトランスフォーマである. それらにより, 可変状態, エラー処理や, ロギングのような異なる機能を容易に付与したり, 組み合わせたりできる. 私はモナドに関するチュートリアルを書こうとは思わないが, ここでは無理やりな例えを用いる: モナドは玉ねぎのようである. (モナドはケーキのようではない.) これにより, 層を意味する.

コアモナドと呼ばれる最も内側あるいは底辺になるモナドが存在する. このコアの頂上に層を重ね, それぞれが新しい特徴を付け加え, 外側あるいは上側に広がっていく. 動機付けのための例として, `ErrorT`トランスフォーマをIOモナドの上に積み重ねよう. 

``` haskell
newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }
type MyStack = ErrorT MyError IO
```

ここで注意してください: ErrorTはただの単純なEitherをモナドでラップしたnewtypeである. newtypeを除去すると, 次のようになる.

``` haskell
type ErrorTUnwrapped e m a = m (Either e a)
```

ある時点において, 実際にMyStackにおいてIOをおこなう必要がある. もしラップしない方法を用いると, 単純である. なぜならば, ErrorTコンストラクタが途中にないためである. しかし, ここでは深入りしないが(これはモナドトタンスフォーマのチュートリアルではない), newtypeラッパは様々な型推論のために必要なのである. そこで解決策はMonadTrans型クラスである. 

``` haskell
class MonadTrans t where
    lift :: Monad m => m a -> t m a
```

始めてこの型注釈を見た時, 反応は混乱のため唖然としたのと, それが実際に何を意味するかについての疑念であった. しかしインスタンスを見ることが非常に役立った:

instance (Error e) => MonadTrans (ErrorT e) where
    lift m = ErrorT $ do
        a <- m
        return (Right a)

行なっていることはIOの中身をRight値でラップし, newtypeラッパを適応しているだけである. これにより, IOにあるアクションを取り出し, それを外あるいは上にあるモナドに"リフト"することができる. 

しかし今, 要点を整理してみる. これは単純な関数において非常によく機能する. 例えば, 

``` haskell
sayHi :: IO ()
sayHi = putStrLn "Hello"

sayHiError :: ErrorT MyError IO ()
sayHiError = lift $ putStrLn "Hello"
```

しかし, コールバックのような何か少しもっと複雑なものを取り上げてみよう:

``` haskell
withMyFile :: (Handle -> IO a) -> IO a
withMyFile = withFile "test.txt" WriteMode

sayHi :: Handle -> IO ()
sayHi handle = hPutStrLn handle "Hi there"

useMyFile :: IO ()
useMyFile = withMyFile sayHi
```

今までのところ問題はないですね? 今例えばError monadにアクセスできるsayHiがあるとしよう:

``` haskell
sayHiError :: Handle -> ErrorT MyError IO ()
sayHiError handle = do
    lift $ hPutStrLn handle "Hi there, error!"
    throwError MyError
```

withMyFileとsayHiErrorを組み合わせた関数を書きたい. 残念なことにGHCはこれをあまり好まない:

``` haskell
useMyFileErrorBad :: ErrorT MyError IO ()
useMyFileErrorBad = withMyFile sayHiError

    Couldn't match expected type `ErrorT MyError IO ()'
                with actual type `IO ()'
```

これがなぜ起こるのでしょうか, またどのようにしてこれに対処できるのでしょうか?

## 直感

ここで何が起こっているかについて外部的な直感を働かせてみよう. `ErrorT`モナドトランスフォーマはIOモナドに対し追加的な機能を与える. その追加的機能をIOモナドに加えるための方法を定義した: Rightコンストラクタを追加し, それら全てをErrorTでラップした. Rightでのラップはここでは"うまく行っているよ"ということであり, このアクションに何も問題はなかったことを意味する. 

今やこれは直感的に意味を成す: IOモナドは何かうまく行かなかった際にMyErrorを返すという概念を持たないため, 常にリフティングにおいて成功する. (注意: これは実行時例外とは何の関係もない, それについては考えないでください.) 一方向性的に, モナドスタックを登ることを保証されただけである.

他の例を見てみよう: Readerモナドである. Readerは周囲にあるいくつかの追加データにアクセス可能である. 内部モナドで実行されているものはその追加情報についてはわからない. したがって, どのようにliftを行えばよいのか? 単にその余剰情報を無視すればよいのである. Writerモナドについてはどうであろうか? 何も書かない. Stateについてはどうか? 何も変更しない. ここではそのパターンを見ている. 

しかし今度は反対方向を試してみよう: Readerの何かを持っており, それをベースモナド(例えばIO)で実行したいとする. ええ, それは機能しませんよね? 追加情報が必要であり, またそれに依存しているが, それは存在しないのである. 追加的な値を与えずに反対方向に行くことはできないのである. 

またはどこかにあるのであろうか? 覚えているでしょうか, 最初にErrorTは内部モナドの単なるラッパであると指摘した. 言い換えると, もし`errorValue :: ErrorT MyError IO MyValue`があれば, `runErrorT`を適用し`IO (Either MyError MyValue)`型の値を得ることができる. これはちゃんと両方向性の変換ではないでしょうか?

完全にはそうではない. もともと`ErrorT MyError IO monad`と, `MyValue`型の値が存在した. 今`Either MyError MyValue`のついた`IO`型のモナドが存在する. したがって, このプロセスは実際に値を変えたが, リフトプロセスは同じ状態に保った. 

しかし, 少し変わった方法を用いてErrorTをアンラップし, 処理をいくつか行い, 再びラップして戻すことができる. 

``` haskell
useMyFileError1 :: ErrorT MyError IO ()
useMyFileError1 =
    let unwrapped :: Handle -> IO (Either MyError ())
        unwrapped handle = runErrorT $ sayHiError handle
        applied :: IO (Either MyError ())
        applied = withMyFile unwrapped
        rewrapped :: ErrorT MyError IO ()
        rewrapped = ErrorT applied
     in rewrapped
```

これはこの章全体で重要な点であるので, しっかり見てみよう. 最初にモナドをアンラップする. これは外の世界に対して, 単にプレーンなIO値であることを意味する. 内部的にはErrorTトランスフォーマから全ての情報を格納した. 今やプレーンなIOを持っているため, 容易にwithMyFileに渡すことができる. withMyFileは内部状態を取り, 変更せずに元に戻す. 最後に, 全てをもとのErrorTにラップして戻す. 

これはmonad-controlにおける全体的なパターンである: モナドトランスフォーマの追加的な情報を値の中に埋め込む. 一度値の中に入れば, 型システムはそれを無視し内部モナドに焦点を当てる. その内部モナドで処理を行った後に, 状態を戻し, 元のモナドスタックを再構築できる. 

## 型

意図的にErrorTトランスフォーマから始めた. なぜならばそれは反転メカニズムが最も簡単なものの1つであるためである. 残念なことに, 他のものはもう少し複雑である. 例えばReaderTを取ってみよう. これは`newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }`で定義される. `runReaderT`を適用するとき, モナド値を返す関数を得る. したがって, それら全てを扱うための追加的機能が必要になる. これはKansasを後にする時である. 

これらの問題を解決するための方法が少し存在する. 過去にはどのパッケージにもないが, タイプファミリを用いた方法を実装した. Anders Kaseorgはずっと直接的な方法をmonad-peelを用いて実装した. そして効率化のため, monad-controlにおいて, Bas van DijkはCPS(継続渡しスタイル)と存在型を用いた. 

<div class="yesod-book-notice">
monad-controlから取ってきたコードは実際にはバージョン0.2に適応される. 0.3では少し変更され, ステートを関連型を用いて明確にし, `MonadControlIO`を`MonadBaseControl`に一般化している. しかし, 概念としては同じである. 
</div>

まず見る型を次のようになる:

``` haskell
type Run t = forall n o b. (Monad n, Monad o, Monad (t o)) => t n b -> n (t o b)
```

これは非常に密度の濃いものであるので, 取り出してみよう. これに対する唯一の"入力"はモナドトランスフォーマtである. Runはどのn, oとbの組み合わせに対しても機能する(これはforallの意味するところである). nとoは両者ともモナドであり, bはそれらに含まれる値である.

Run関数の左側であるt n bはモナドトランスフォーマであり, nモナドをラップし, b値を持つ. よって例えば, それはMyTrans FirstMonad MyValueのようであったりする. そしてトランスフォーマは内部に"ポップ"され, 全く新しいモナドがコアに来る. 言い換えれば, FirstMonad (MyTrans NewMonad
MyValue)のようになる.

一見するとかなり怖いように見えるが, しかし実際には思うほどよそよそしいものではない: これは本質的にはErrorTで行ったものと同じである. ErrorTが外側にありIOをラップしたものから始まり, Eitherを含んでいるIOで終わった. 推測してみてください: Eitherを表現するための他の方法が`ErrorT MyError Identity`である. よって本質的には, IOを内部に持ってきてIdentityはその場に留めているだけである. Runにおいても同じことを行なっている: FirstMoandを外側に持っていき, それをNewMonadで置き換える.

<div class="yesod-book-notice">
さあビールを飲むのにちょうど良いときである.
</div>

さあ, きっとどこかにたどり着いてることでしょう. もしそれらRun関数の1つにでもアクセス可能であれば, それを使ってsayHello関数のErrorTを剥がし, withMyFileに渡すことができる. undefinedの奇術により, 次のようなことを試すことができる.

``` haskell
errorRun :: Run (ErrorT MyError)
errorRun = undefined

useMyFileError2 :: IO (ErrorT MyError Identity ())
useMyFileError2 =
    let afterRun :: Handle -> IO (ErrorT MyError Identity ())
        afterRun handle = errorRun $ sayHiError handle
        applied :: IO (ErrorT MyError Identity ())
        applied = withMyFile afterRun
     in applied
```

これは非常に前の例と類似している. 実際にerrorRunはrunErrorTとほとんど同様に機能している. しかし, まだ2つの問題がある. どこからerrorRun値を得るのかわからないし, その後に, まだもとのErrorTを再構築する必要がある.

## MonadTransControl

明らかに前の例のように特別な場合, ErrorTトランスフォーマの知識を用いて, 型を打ち独自のRun関数を作ることができる. しかし, 本当にしたいことは多くのトランスフォーマに対する一般的な解決策である. この点において, 型クラスが必要であることがわかるであろう. 

そこで, 必要なものについて整理しましょう: Run関数へのアクセスと, その後にもとのトランスフォーマを再構築するための方法. その結果, MonadTransControlが誕生し, これはliftControlという1つのメソッドを持つ.

``` haskell
class MonadTrans t => MonadTransControl t where
    liftControl :: Monad m => (Run t -> m a) -> t m a
```

これについて詳しく見てみよう. liftControlは関数(今書く予定の関数)を引数に取る. その関数はRun関数を与えられ, あるモナド(m)の値で返される必要がある. liftControlはその関数の結果を取り, 元のトランスフォーマをトップレベルに戻す.

``` haskell
useMyFileError3 :: Monad m => ErrorT MyError IO (ErrorT MyError m ())
useMyFileError3 =
    liftControl inside
  where
    inside :: Monad m => Run (ErrorT MyError) -> IO (ErrorT MyError m ())
    inside run = withMyFile $ helper run
    helper :: Monad m
           => Run (ErrorT MyError) -> Handle -> IO (ErrorT MyError m ())
    helper run handle = run (sayHiError handle :: ErrorT MyError IO ())
```

近いが, 本当に思ったものではない. 2重モナドは何でしょうか? ええ, 最後の部分から始めましょう: sayHello handleはErrorT MyError IO ()型の値を返す. これはすでに既知のものであり. 驚くに値しない. 少し驚くべきことは(少なくとも私にとっては), 次の2ステップである. 

始めにrunをその値に適用する. 前に論じたように, 結果としてIO内部モナドは外に出され, 他の任意のモナドに置き換えられる(ここではmで表す). IO (ErrorT MyError m())を取得することになる. オーケーです. そしてwithMyFileを適用後, 同じ結果を得ることになる. 驚くに値しない.

最後のステップは, 正しく理解するのに長い時間がかかった. 元のトランスフォーマを再構築すると言ったのを覚えているだろうか? ええ, 実際にそうします: ちょうど今持っている全ての上に置きます. その結果, 前の型 IO (ErrorT MyError m ())に対しErrorrT MyErrorが手前に来る. 

ええ, それは全く意味のないように見えませんか? ええ, ほとんどそうです. しかし, "m"はIOを含めどんなモナドにもなれることを忘れてはいけない. もし, そのように考えれば, `ErrorT MyErrorT IO (ErrorT MyErrorT IO ())`を得る. これはかなり`m (m a)`に近い. そして, 実際にはプレーンな`m a`を得たいのである. 幸い, 運が良い: 

``` haskell
useMyFileError4 :: ErrorT MyError IO ()
useMyFileError4 = join useMyFileError3
```

そして, この方法は一般的であり, Basは情け深いことにヘルパ関数を定義してくれた.

``` haskell
control :: (Monad m, Monad (t m), MonadTransControl t)
        => (Run t -> m (t m a)) -> t m a
control = join . liftControl
```

そこて書く必要のあるのは:

``` haskell
useMyFileError5 :: ErrorT MyError IO ()
useMyFileError5 =
    control inside
  where
    inside :: Monad m => Run (ErrorT MyError) -> IO (ErrorT MyError m ())
    inside run = withMyFile $ helper run
    helper :: Monad m
           => Run (ErrorT MyError) -> Handle -> IO (ErrorT MyError m ())
    helper run handle = run (sayHiError handle :: ErrorT MyError IO ())
```

これを少し短くすると:

``` haskell
useMyFileError6 :: ErrorT MyError IO ()
useMyFileError6 = control $ \run -> withMyFile $ run . sayHiError
```

## MonadControlIO 

MonadTransクラスはliftメソッドを与え, これによりモナドのスタックにおいてアクションを1レベル上げることができる. また, MonadIOクラスが存在し, これはliftIOを与え, これによりIOアクションがスタックにおいて望むだけ外に持ち上げることができる. monad-controlにも同じブレークダウンが存在する. しかし最初にRunのための系が必要である:

``` haskell
type RunInBase m base = forall b. m b -> base (m b)
```

トランスフォーマを扱う代わりに2つのモナドを扱っている. baseは下層モナドであり, mははその上にあるスタックである. RunInBaseはスタック全体の値を取り, そのベースを取り出し, 外側に置く関数である. Run型と異なり, それを任意モナドと置き換えないが, もとのモナドと置き換える. より具体的な型を扱うために:

``` haskell
RunInBase (ErrorT MyError IO) IO = forall b. ErrorT MyError IO b -> IO (ErrorT MyError IO b)
```

これはこれまでに見てきたものとかなり似通っており, 唯一の違いは特別な内部モナドを扱いたいことである. MonadControlIOクラスは実際にMonadControlTransのRunInBaseを用いた拡張に過ぎない. 

``` haskell
class MonadIO m => MonadControlIO m where
    liftControlIO :: (RunInBase m IO -> IO a) -> m a
```

簡単に言うと, liftControllIOはRunInBaseを受け取る関数を取る. RunInBaseはモナドを剥がしたただのIOにするために使われ, liftControlIOは再び全てを構築し直す. MonadControlTransと同様に, それはヘルパ関数とともに来る. 

``` haskell
controlIO :: MonadControlIO m => (RunInBase m IO -> IO (m a)) -> m a
controlIO = join . liftControlIO
```

これを用い, 容易に前の例を書き直せる:

``` haskell
useMyFileError7 :: ErrorT MyError IO ()
useMyFileError7 = controlIO $ \run -> withMyFile $ run . sayHiError
```

利点として, これは容易に複数のトランスフォーマにスケールされる:

``` haskell
sayHiCrazy :: Handle -> ReaderT Int (StateT Double (ErrorT MyError IO)) ()
sayHiCrazy handle = liftIO $ hPutStrLn handle "Madness!"
```

``` haskell
useMyFileCrazy :: ReaderT Int (StateT Double (ErrorT MyError IO)) ()
useMyFileCrazy = controlIO $ \run -> withMyFile $ run . sayHiCrazy
```

## Real Life Example

このコードで何か現実世界の問題を解いてみよう. おそらく最も大きな動機づけとなる使い方は, トランスフォーマスタックにおける例外処理であろう. 例えば, 例外が投げられた時, 自動的にクリーンアップコードを実行したいとしよう. もし, これが通常のIOコードであれば, 次を使うであろう:

``` haskell
onException :: IO a -> IO b -> IO a
```

しかし, もしErrorTモナドにいる場合, アクションを渡したりクリーンアップすることができない. そこで, controlIOが救助のために駆けつける:

``` haskell
onExceptionError :: ErrorT MyError IO a
                 -> ErrorT MyError IO b
                 -> ErrorT MyError IO a
onExceptionError action after = controlIO $ \run ->
    run action `onException` run after
```

Doubleを保存するためのメモリを割り当てたいとしよう. IO monadにおいては, alloca関数を用いればよかった. 再度, 今回の解決策も単純である:

``` haskell
allocaError :: (Ptr Double -> ErrorT MyError IO b)
            -> ErrorT MyError IO b
allocaError f = controlIO $ \run -> alloca $ run . f
```

## Lost State

少しonExceptionErrorに巻き戻りましょう. これはonExceptionを水面下で用いており, 次のような型注釈を持つ : `IO a -> IO b -> IO a` 質問させてください: 出力においてbはどうなったのか? ええ, それは完全に無視されている. しかし, これは少し問題を生じる. 結局, トランスフォーマの状態に関する情報は内部モナドの中に保存しているのである. もし, それが無視されれば, 本質的にモナド副作用を同様に無視していることになる!

そして答えとしては, ええ, これはmonad-controlで起こっている. ある関数はモナド副作用を捨てている. これはBasによって, 関連する関数についてのコメントでよく言い表されている: [引用]

"開放"計算においてmのどんなモナド副作用も無視されることに注意せよ; それはIOにおいて副作用のためだけに実行される. 

実践的には, monad-controlはたいてい正しいことをしてくれるでろうが, 消えてしまう副作用もあることに注意せよ.

## More Complicated Cases

今までのところトリックを機能させるために, 値を操作するための完全なアクセスを与えるための関数が必要であった. 時々, このようでない場合もある. 例えば:

``` haskell
addMVarFinalizer :: MVar a -> IO () -> IO ()
```

この場合, 終了関数に値を持つ必要がない. 直感的には, 最初に注意すべき点としては, モナド副作用をとらえる方法がないということである. そこでどのようにこのようなものをコンパイルすればよいのであろうか? ええ, それが持っている全ての状態に関する情報を捨てるように明確に伝える必要がある. 

``` haskell
addMVarFinalizerError :: MVar a -> ErrorT MyError IO () -> ErrorT MyError IO ()
addMVarFinalizerError mvar f = controlIO $ \run ->
    return $ liftIO $ addMVarFinalizer mvar (run f >> return ())
```

同じモジュール他の例としては:

``` haskell
modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
```

ここでは, 2つ目の引数に対する戻り値の制限がある: それはその関数に渡される引数と, 最後の戻り値の3つ組である必要がある. 残念なことに, modyfyMVarをErrorTで使えるようにするための簡単なラッパを書く方法を見つけられない. 代わりに, この場合, modyfyMVarの定義をコピーし, 次のように変更した:

``` haskell
modifyMVar :: MVar a
           -> (a -> ErrorT MyError IO (a, b))
           -> ErrorT MyError IO b
modifyMVar m io =
  Control.Exception.Control.mask $ \restore -> do
    a      <- liftIO $ takeMVar m
    (a',b) <- restore (io a) `onExceptionError` liftIO (putMVar m a)
    liftIO $ putMVar m a'
    return b
```