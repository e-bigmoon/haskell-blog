# Yesod’s Monads

この本を読み進めるにつれ, 多くのモナドが現れてきた: `Handler`, `Widget`, そして, `YesodDB`(Persistentのための)などである. 多くのモナドを同様に, それぞれは, 特有の機能を持つ: `handler`はリクエストにアクセスし, レスポンスを送信できるようにする. `Widget`は, HTML, CSS, そして, Javascriptを含む. `YesodDB`はデータベースのクエリを発行することを可能にする. Model-View-Controller(MVC)の用語で言うと, `YesodDB`はモデルであり, `Widget`はビュー, そして, `Handler`はコントローラと考えられる.

これまでのところ, これらのモナドを使うための非常に率直な方法を紹介してきた: メインのハンドラは`Handler`で実行され, `runDB`を用いて`YesodDB`クエリを実行し, そして, `defaultLayout`を用いて`Widget`を返し, `Widget`は`toWidget`を呼ぶことで次々に作られた. 

しかし, これらの型をより深く理解できれば, より面白い結果が得られるだろう. 

## モナドトランスフォーマ

Shrek- more or less
モナドは玉ねぎのようである. モナドはケーキのようではない.

Yesodのモナドの核に迫る前に, モナドとランスフォーマの理解が少し必要になる(もし, モナドとランスフォーマについて少し知っているのならば, この節は読み飛ばしてもよいだろう.). 異なるモナドは異なる機能を持つ: `Reader`は計算時において, いくつかのデータに対するread-onlyアクセスを与え, `Error`は, 計算を短絡する, などである.

しかし, しばしば, これらのいくつかの特徴を組み合わせたいと思うだろう. 結局, どうしてエラーアウトし得る設定変数に対し, read-onlyアクセスを付与した計算ができない必要があるだろうか? これに対する1つの方法は, `ReaderError`のような新しいモナドを書くことである. しかし, これは非常に複雑になるという明から欠点を持つ: つまり, 1つの可能な組み合わせ毎に新しいモナドを書く必要が出てくる.

代わりに, モナドトランスフォーマが存在する. `Reader`に加え, `readerT`があり, それは, 他のモナドにreader機能を与える. すると, ReaderErrorは, (概念的には)次のように表せる.

``` haskell
type ReaderError = ReaderT Error
```

設定変数にアクセスするために, `ask`関数を用いる. しかし, 計算の短絡にはどうするのであろうか? `throwError`を用いたいが, それは正しくは機能しない. 代わりに, 呼び出しを`lift`し, 次のモナドにつなげるのである. 言い換えると:

``` haskell
throwError :: errValue -> Error
lift . throwError :: errValue -> ReaderT Error
```

ここに, いくつかの要点を挙げる:

- トランスフォーマは, 既存のモナドに機能を追加するために用いられる.

- トランスフォーマは, 常に既存のモナドをラップしなければならない.

- ラップされたモナドが利用可能な機能は, モナドトランスフォーマだけでなく, ラップされた内部モナドにも依存する.

最後の点における最もよい例は, `IO`モナドである. `IO`の周りにトランスフォーマの層がいくつあったとしても, 核には`IO`があり, これらのモナドトランスフォーマの層において, I/Oを実行できることを意味する. `liftIO $ putStrLn "Hello There!"`のように見えるコードをよく目にすることだろう.


## 3つのトランスフォーマ

<div class=yesod-book-notice>
Yesod初期バージョンにおいては, `Handler`や`Widget`はより魔術的で難しいものであった. バージョン1.2から, 物事はずっと単純になった. もし, 偽物のとランスフォーマやサブサイトのパラメータに関する難しいものを読んだことを覚えているならば, 安心してもよい: 戸惑う必要はない, 物事は実際に少し変わったのである. パージステントの話も同様にずっと単純である. 
<div>

これまでに, 2つのトランスフォーマについて論じた: `Handler`と`Widget`である. より一般的な`handlerT`や, `WidgetT`のようなアプリケーション特有の同義語が存在することを思い出そう. これらの各々は, 2つの型パラメータを取る: ファウンデーションデータ型と, ベースモナドである. 最も一般的に用いられるベースモナドは, `IO`である. persistentでは, `PersistStore`と呼ばれる型クラスがある. この型クラスは`get`のようにデータベースに対して実行できる, 全ての原始的関数を定義している. この型クラスには, persistentによって支持されたバックエンドのデータベース毎にインスタンスが存在する. 例えば, SQLデータベースについては, `SqlBackend`と呼ばれるデータ型が存在する. そして, 標準的`ReaderT`
変換を用いて, `SqlBackend`値を全ての演算に与える. このことは, SQLデータベースを`MonadIO`のインスタンスであるベースモナドを用いて実行できることを意味する. ここで覚えておくべき点は, Persistent変換を`Handler`や`Widget`の上に載せることができる点である.

関連するPersistent変換を言及するのを容易にするために, yesod persistentパッケージは, `YesodPersistentBackend`関連型を定義する. 例えば, `MyApp`というサイトがあり, それがSQLを用いていれば, `type instance YesodPersistentBackend App = SqlBaclend`のようなものを定義するだろう. そして利便性のために, `YesodDB`という同義語が次のように定義できる:

``` haskell
type YesodDB site = ReaderT (YesodPersistBackend site) (HandlerT site IO)
```

するとデータベースの操作は次の`YesodDB MyApp SomeResult`ような型で表せる. これらを実行するために, 標準的なPersistenyのアンラップ関数(`runsqlPool`のような)を用いる. これを自動化するために, `runDB`関数を用いる. これらを全てまとめると, ハンドラ内でデータベース操作を実行できる. 

Yesodコードの大部分, 特にこれまでのところこの本においては, ウィジットは, 操作のないコンテナであり, 単にHTML, CSS, そして, Javascriptを組み合わせるものと扱われてきた. しかし実際には, `Widget`は`Handler`ができることは, `handlerToWidget`関数を用いて何でもできる. 例えば, `Widget`において`handlerToWidget . runDB`のようなものを用いて, データベースクエリを実行できる. 

## 例 : データベースによって駆動されたナブバー

この新しい知識を行動に移しましょう. `Widget`を作り, データベースの中身に基づいて, 結果を生成したい. 以前は, 方法としてデータをハンドラに読み込み, そのデータをウィジットに渡していた. 今回は, データの読み込みをウィジットそれ自身で行う. これはモジュール方式の恩恵である. なぜならば, この`Widget`は用いたいどの`Handler`においても使うことができ, データベースの中身を通過する必要がないためである. 

``` haskell
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.Logger    (runNoLoggingT)
import           Data.Text               (Text)
import           Data.Time
import           Database.Persist.Sqlite
import           Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Link
    title Text
    url Text
    added UTCTime
|]

data App = App ConnectionPool

mkYesod "App" [parseRoutes|
/         HomeR    GET
/add-link AddLinkR POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB db = do
        App pool <- getYesod
        runSqlPool db pool

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <form method=post action=@{AddLinkR}>
            <p>
                Add a new link to
                <input type=url name=url value=http://>
                titled
                <input type=text name=title>
                <input type=submit value="Add link">
        <h2>Existing links
        ^{existingLinks}
    |]

existingLinks :: Widget
existingLinks = do
    links <- handlerToWidget $ runDB $ selectList [] [LimitTo 5, Desc LinkAdded]
    [whamlet|
        <ul>
            $forall Entity _ link <- links
                <li>
                    <a href=#{linkUrl link}>#{linkTitle link}
    |]

postAddLinkR :: Handler ()
postAddLinkR = do
    url <- runInputPost $ ireq urlField "url"
    title <- runInputPost $ ireq textField "title"
    now <- liftIO getCurrentTime
    runDB $ insert $ Link title url now
    setMessage "Link added"
    redirect HomeR

main :: IO ()
main = runNoLoggingT $ withSqlitePool "links.db3" 10 $ \pool -> liftIO $ do
    runSqlPersistMPool (runMigration migrateAll) pool
    warp 3000 $ App pool
```

特に`existingLink`関数に注意を払いなさい. 必要なこと全ては, `handlerToWidget . runDB`を通常のデータベース操作に適用することである. そして, `getHomeR`から, `existingLinks`を通常の`Widget`のように扱い, 特別なパラメータは全く用いなかった. このアプリの結果については, 図を参照しなさい. 

![](https://www.yesodweb.com/book/image/navbar "navbar")

## 例 : リクエスト情報

同様に`Widget`内にリクエスト情報を取得できる. ここでは, GETパラメータに基づき, リストのソート順を決定できる.

``` haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.List (sortBy)
import           Data.Ord  (comparing)
import           Data.Text (Text)
import           Yesod

data Person = Person
    { personName :: Text
    , personAge  :: Int
    }

people :: [Person]
people =
    [ Person "Miriam" 25
    , Person "Eliezer" 3
    , Person "Michael" 26
    , Person "Gavriella" 1
    ]

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            <a href="?sort=name">Sort by name
            |
            <a href="?sort=age">Sort by age
            |
            <a href="?">No sort
        ^{showPeople}
    |]

showPeople :: Widget
showPeople = do
    msort <- runInputGet $ iopt textField "sort"
    let people' =
            case msort of
                Just "name" -> sortBy (comparing personName) people
                Just "age"  -> sortBy (comparing personAge)  people
                _           -> people
    [whamlet|
        <dl>
            $forall person <- people'
                <dt>#{personName person}
                <dd>#{show $ personAge person}
    |]

main :: IO ()
main = warp 3000 App
```

この場合, `handlerToWidget`すら呼び出す必要がないことに注意しなさい. その理由は, Yesodに含まれる多くの関数が, `MonadHandler`型クラスを用いて, 自動的に`Handler`と`Widget`の機能を果たすためである. 実際に, `MonadHandler`によって, これらの関数は多くの共通のモナド変換により"自動的にリフト"される.
しかし, 望むのなら, `runInputGet`の呼び出しを`handlerToWidget`を用いてラップすることができるが, その場合も全く同じ動作をする.


## パフォーマンスとエラーメッセージ

<div class=yesod-book-notice>
この節は課外と考えて良い. Yesodの裏にあるデザイン同期についてであるが, Yesodの使用上においては, 必要ではない. 
<div>

この点において, 少し混乱するかもしれない. 上に述べたように, `Widget`シノニムは`Hanlder`ではなく, `IO`をベースモナドとして用いる. それでは, どのようにして`Widget`は`Handler`操作を実行するのであろうか? そして, どうして`Widget`を`Handler`の上部のトランスフォーマにし, `lift`を用いることをしないで, このような特別な`handlerToWidget`を用いるのであろうか. そして, 最後に, `Widget`と`Handler`は共に`MonadResource`のインスタンスになることを述べた. もし, `MonadResource`に馴染みがあるならば, なぜ`ResourceT`がモナドトランスフォーマスタックに現れないのか不思議に思うかも知れない. 

この問題の真相としては, これらのモナドトランスフォーマすべてに対し, 取り得るずっと簡単な(実装面において)方法があるということである. `Handler`は単なる`IO`ではなく, `ResourceT IO`の上部にあるトランスフォーマになり得, そのほうが多少正確であった. そして, `Widget`は`Handler`の上部に重ねることもできた. 最終的な結果は, 次のようになるだろう.

``` haskell
type Handler = HandlerT App (ResourceT IO)
type Widget  = WidgetT  App (HandlerT App (ResourceT IO))
```

直接とランスフォーマ型を用いる代わりに, より親切な型シノニムを大部分で用いているため, そんなに悪くは見えないだろう. 問題点は, 根底にあるトランスフォーマが漏出するたびに, 大きな型シノニムは信じられないくらい混乱を招くものとなることである. 漏出が最も起こりやすい場面は, エラーメッセージにおいてであり, おそらくそこで非常に混乱するだろう! (他の場面では, サブサイトで作業をする場合であり, それもたまに混乱するものとなるだろう.)

他の心配事は, 各モナドとランスフォーマのレイヤはいくらかの動作ペナルティを付加することである. これは, 動作しているI/Oに比較すれば無視できるほどであるが, オーバーヘッドは存在する.

そこで, 正しく層にしたトランスフォーマを用いる代わりに, 各`HandlerT`と`WidgetT`を1つのレベルのトランスフォーマに平坦化する. ここでは, 用いられる方法を高みから外観する. 

- `HandlerT`は実際には`ReaderT`モナドである. (エラーメッセージを明確にするために異なる名称を与えただけである。) これは, `HandlerData`型に対するリーダーであり, リクエスト情報とほかの不変コンテンツを含んでいる.

- さらに, `HandlerData`は, `GHState`(歴史的な理由でよくない名前である)に対する`IORef`を持ち, それはハンドラ(例えば, セッション変数)の過程で変化するデータを保持する. `StateT`の代わりに`IORef`の方法を用いるのは,`IORef`は実行時例外が投げられても変化した状態を持ち続けるためである.

- `ResourceT`モナドは実質的には, `IORef`を持つ`ReaderT`モナドトランスフォーマである. この`IORef`は, 実行されなければならないすべてのクリーンアップ動作に関する情報を含んでいる.(これは, `InternalState`と呼ばれる) 参照を保持する別々のトランスフォーマのレイヤを持つ代わりに, `HandlerData`の自己参照を行う.(その通り, ここで`IORef`を用いる理由は, 実行時例外のためでもある.)

- `WidgetT`は本質的には, 単に`HandlerT`が行う全てのものの上にある`WriterT`である. しかし, `HandlerT`は, 単なる`ReaderT`であるため, 容易に2つの側面を1つのトランスフォーマに圧縮でき, ` newtype WidgetT site m a = WidgetT (HandlerData → m (a, WidgetData))`のようなものである.

この部分をより理解したければ, `Yesod.Core.Type`における`HandlerT`と`WidgetT`の定義を見てみるといいだろう.

## 新しいモナドトランスフォーマを追加する.

アプリケーションの一部に, 自身のモナドランスフォーマを追加したい時があるかもしれない. 動機づけのための例として, Hackageおける[monadcryptorandom](https://www.stackage.org/package/monadcryptorandom)パーッケージを考えてみよう. これは, モナドに対し, 暗号論的に安全な乱数を生成する`MonadCRandom`型クラスと, その型クラスの具体的なインスタンスとして`CRandT`を定義する. ランダムなバイトストリングを生成するコードを以下の例で書いてみよう.

``` haskell
import Control.Monad.CryptoRandom
import Data.ByteString.Base16 (encode)
import Data.Text.Encoding (decodeUtf8)

getHomeR = do
    randomBS <- getBytes 128
    defaultLayout
        [whamlet|
            <p>Here's some random data: #{decodeUtf8 $ encode randomBS}
        |]
```

しかし, これは次の行を伴ったエラーメッセージとなる:

``` bash
    No instance for (MonadCRandom e0 (HandlerT App IO))
      arising from a use of ‘getBytes’
    In a stmt of a 'do' block: randomBS <- getBytes 128
```

どのようにすれば, そのようなインスタンスを取得できるであろうか. ひとつの方法として, `getBytes`を呼ぶ際に, 単純に`CRand`モナドトランスフォーマを用いる方法がある. そのようにするための完全な例は以下のようになる:

``` haskell
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
import Yesod
import Crypto.Random (SystemRandom, newGenIO)
import Control.Monad.CryptoRandom
import Data.ByteString.Base16 (encode)
import Data.Text.Encoding (decodeUtf8)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    gen <- liftIO newGenIO
    eres <- evalCRandT (getBytes 16) (gen :: SystemRandom)
    randomBS <-
        case eres of
            Left e -> error $ show (e :: GenError)
            Right gen -> return gen
    defaultLayout
        [whamlet|
            <p>Here's some random data: #{decodeUtf8 $ encode randomBS}
        |]

main :: IO ()
main = warp 3000 App
```

今や行っていることは, `CRandT`トランスフォーマを`HandlerT`トランスフォーマの上にレイヤリングすることである. 他の方法では機能しない: Yesodそれ自身は, 最終的に`CRand`トランスフォーマをアンラップする必要があるが, そうするための知識がないのである. これは`Persistent`でとった方法と同じであることに留意しなさい: トランスフォーマは`HandlerT`の上に位置する. 


しかし, この方法には2つの短所が存在する.

1. 乱数を扱うごとに, この代わりのモナドに入り込む必要がある.

2. それは非効率的である: このモナドに入るたびに、新しい乱数を作る必要がある.

2つ目の点については, 乱数のシードを`IORef`のような変化可能な参照であるファウンデーションデータ型に保存し, `CRandT`トランスフォーマに入るたびに, アトミックにサンプリングすることで回避できる. しかし, さらにワンステップ深入りし, `Handler`モナド自身を`MonadCRandom`のインスタンスにできる! 実は少し関係ないが, コードを見てみよう.

``` haskell
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
import           Control.Monad              (join)
import           Control.Monad.Catch        (catch, throwM)
import           Control.Monad.CryptoRandom
import           Control.Monad.Error.Class  (MonadError (..))
import           Crypto.Random              (SystemRandom, newGenIO)
import           Data.ByteString.Base16     (encode)
import           Data.IORef
import           Data.Text.Encoding         (decodeUtf8)
import           Yesod

data App = App
    { randGen :: IORef SystemRandom
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    randomBS <- getBytes 16
    defaultLayout
        [whamlet|
            <p>Here's some random data: #{decodeUtf8 $ encode randomBS}
        |]

instance MonadError GenError Handler where
    throwError = throwM
    catchError = catch
instance MonadCRandom GenError Handler where
    getCRandom  = wrap crandom
    {-# INLINE getCRandom #-}
    getBytes i = wrap (genBytes i)
    {-# INLINE getBytes #-}
    getBytesWithEntropy i e = wrap (genBytesWithEntropy i e)
    {-# INLINE getBytesWithEntropy #-}
    doReseed bs = do
        genRef <- fmap randGen getYesod
        join $ liftIO $ atomicModifyIORef genRef $ \gen ->
            case reseed bs gen of
                Left e -> (gen, throwM e)
                Right gen' -> (gen', return ())
    {-# INLINE doReseed #-}

wrap :: (SystemRandom -> Either GenError (a, SystemRandom)) -> Handler a
wrap f = do
    genRef <- fmap randGen getYesod
    join $ liftIO $ atomicModifyIORef genRef $ \gen ->
        case f gen of
            Left e -> (gen, throwM e)
            Right (x, gen') -> (gen', return x)

main :: IO ()
main = do
    gen <- newGenIO
    genRef <- newIORef gen
    warp 3000 App
        { randGen = genRef
        }
```  

これは, 実際にいくつかの異なる概念に帰着する.

1. `App`データ型を`IORef SystemRandom`のフィールドを持つように変更する.

2. 同様に, `main`関数を`IORef SystemRandom`を生成するように変更する. 

3. `getHomeR`関数は, ずっと単純になる: 今や, トランスフォーマを使わずに, 単に`getBytes`を呼ぶだけでよい.

4. しかし, `MonadCRandom`インスタンスが必要になるという複雑さが増す. これはYesodについての本であり, `monadcryptorandom`についての本ではないため, このインスタンスについては詳細は述べないが, それを検証し, もし興味があれば, `CRandT`のインスタンスと比較することを推奨する. 

幸いなことに, これは重要な点を理解するのに役立つ: `HandlerT`トランスフォーマの力である. 単にリード環境を与えるだけで, 変化可能な参照に頼ることで, `StateT`トランスフォーマを再作成できる. 実際に, もし実行時エラーに対し基底にある`IO`モナドに依存するとしたら, この抽象化を用いれば大部分を`ReaderT`, `WriterT`, `StateT`, そして, `ErrorT`で実装できる.   

## まとめ

もしこの章を完全に無視したとしても, `Yesod`を非常にうまく使うことはできる. Yesodモナドがどのようにして相互作用するかを理解することの利点は, よりクリーンなモジュラーコードを生成できる点にある. 任意のアクションを`Widget`で実行できることは, 強力な道具になり得, どのようにしてPersistentと`Handler`コードが相互作用するのかを理解することは, アプリケーションにおいて, より情報性に富んだデザイン決定をするのに役立つ.