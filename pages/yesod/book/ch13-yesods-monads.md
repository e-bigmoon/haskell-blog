---
title: Yesod のモナドたち
date: 2019/01/21
---

これまでの章で `Handler`、`Widget`、`YesodDB` (Persistent のための) などのモナドが現れました。これらのモナドは一般的なモナドと同様に、各モナドごとに特有の機能が備わっています。`Handler` はリクエストへのアクセスやレスポンスの送信を許可し、`Widget` は HTML、CSS、Javascript を含み、 `YesodDB` はデータベースのクエリを発行できます。Model-View-Controller (MVC) の用語で言うと `YesodDB` はモデル、`Widget` はビュー、そして `Handler` はコントローラになります。

これまでのところ、これらのモナドを使うための非常に率直な方法を紹介してきました。すなわち、メインのハンドラは `Handler` で実行され、`runDB` を使って `YesodDB` クエリを実行し、`toWidget` を呼び出すことで作られた `Widget` を `defaultLayout` で返します。

しかし、これらの型をより深く理解できれば、より面白い知見が得られるでしょう。

## モナドトランスフォーマー

> Shrek- more or less <br>
> モナドは玉ねぎのようである. モナドはケーキのようではない.

Yesod のモナドに進む前にモナドトランスフォーマーを少し勉強しましょう。(もし、モナドトランスフォーマーについて詳しければ、この節は読み飛ばしても良いでしょう)。異なるモナドには異なる機能があります。例えば、`Reader` モナドは計算時にデータに対する読み取り専用アクセスを提供し、`Error` モナドは計算の短絡機能を提供します。

これらのいくつかの機能を組み合わせたいと思うことは良くあることでしょう。例えば、任意のタイミングでエラーが発生するかもしれない計算に、読み取り専用の設定変数を加えても良いはずですよね？これを実現する1つの方法は `ReaderError` のような新しいモナドを定義することですが、これは指数関数的に複雑になるという明からな欠点があります。つまり、1つの可能な組み合わせ毎に新しいモナドを書く必要が出てきてしまいます。

この問題を解決する方法としてモナドトランスフォーマーがあります。`Reader` に加えて、他の任意のモナドに Reader モナドの機能を付与する `ReaderT` があります。これを使えば、`ReaderError` は (概念的には) 次のように表せます。

```haskell
type ReaderError = ReaderT Error
```

設定変数にアクセスするために `ask` 関数を利用します。しかし、計算を短絡させるにはどうすれば良いでしょうか？つまり、`throwError` を利用したいのですが、これだけでは上手くいきません。代わりに、呼び出しを `lift` し、次のモナドに持ち上げるのです。つまり、以下のようになります。

```haskell
throwError :: errValue -> Error
lift . throwError :: errValue -> ReaderT Error
```

ここでの要点は以下の3点です。

- トランスフォーマーは、既存のモナドに機能を追加するために使われます。
- トランスフォーマーは、常に既存のモナドをラップしなければなりません。
- ラップされたモナドで利用可能な機能は、モナドトランスフォーマだけでなく、ラップされている内部のモナドにも依存します。

最後の点における最もよい例は `IO` モナドです。`IO` の周りにトランスフォーマの層がいくつあったとしても核には `IO` があるため、モナドトランスフォーマーの任意の層で I/O を実行できることを意味します。`liftIO $ putStrLn "Hello There!"` のようなコードを良くみかけますよね。

## 3つのトランスフォーマー

<div class="yesod-book-notice">
Yesod 初期バージョンでは `Handler` や `Widget` はより魔術的で難しいものでしたが、バージョン1.2からはとてもシンプルになりました。もし、過去に偽物のトランスフォーマーやサブサイトのパラメータに関する難しいものを読んだことを覚えている読者は安心してください。もう大丈夫です。persistent の話も同じようにシンプルになりました。
</div>

今までの章で、`Handler` と `Widget` の2つのトランスフォーマーについて議論しました。これらは、アプリケーション固有のエイリアスとして、より一般的な `HandlerT` や `WidgetT` によって定義されていました。このトランスフォーマーは2つの型パラメータを取ります。1つはファウンデーションデータ型で、もう一つはベースモナドです。多くの場合、ベースモナドには `IO` が使われます。

persistent では `PersistStore` と呼ばれる型クラスがあります。この型クラスは `get` のような、データベースに対して実行可能な全てのプリミティブな操作を定義しています。この型クラスには persistent でサポートしているバックエンドのデータベース毎にインスタンスが定義されています。例えば、SQL データベースには `SqlBackend` と呼ばれるデータ型があります。そして、全ての操作で `SqlBackend` の値が使えるようにするために、標準的な `ReaderT` トランスフォーマーを利用します。これは、`MonadIO` のインスタンスであれば任意のベースモナドで、SQL データベースを実行できることを意味します。ここで大切なのは、Persistent トランスフォーマーの層を `Handler` や `Widget` の上に載せることができるという点です。

Persistent トランスフォーマーへの参照をシンプルにするため、yesod-persistent パッケージは `YesodPersistentBackend` 関連型を定義しています。例えば、`MyApp` というサイトで SQL を利用する場合は `type instance YesodPersistentBackend MyApp = SqlBackend` のように定義されるでしょう。そして利便性のために `YesodDB` という型シノニムが以下のように定義されます。

```haskell
type YesodDB site = ReaderT (YesodPersistBackend site) (HandlerT site IO)
```

このエイリアスを使えば、データベースのアクションは `YesodDB MyApp SomeResult` ような型で表すことができます。これらを実行するために、標準的な Persistent のアンラップ関数 (`runSqlPool` のようなもの) を使ってアクションを実行し、通常の `Handler` を取得します。この作業を自動化するために `runDB` 関数があります。これらを全てまとめると、ハンドラ内でデータベース操作を実行できるようになります。

Yesod コードの大部分、特に本書のこれまでの内容では、ウィジェットはアクションのないコンテナであり、単に HTML、CSS、Javascript を組み合わせるものとして扱ってきました。しかし、実際には `Widget` は `Handler` ができることは `handlerToWidget` 関数を使って何でも実行できます。例えば、`Widget` 内部でも `handlerToWidget . runDB` のようにすれば、データベースクエリを実行できます。

## 例: データベース駆動のナビゲーションバー

この新しい知識を使って何かしてみましょう。ここでは データベースのコンテンツに基づいた出力を生成するような `Widget` を作ってみましょう。前回のアプローチは、ハンドラでデータを読み込み、ウィジェットに渡していました。今回は `Widget` でデータの読み込みを行います。データベースのコンテンツをハンドラから受け取る処理を `Widget` に任せることで、このウィジェットを任意の `Handler` で使えるようになり、モジュラリティが高まります。

```haskell
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

上記のコードの中でも特に `existingLink` 関数をしっかりと確認してください。ここで、必要なのは `handlerToWidget . runDB` を通常のデータベースアクションに適用することです。そして、`existingLinks` は通常の `Widget` と同じように扱うことができるため、`getHomeR` から呼び出す際に、特別な引数は何も必要ありません。このアプリの結果については、以下の画像を見てください。

![navbar](https://www.yesodweb.com/book/image/navbar "navbar")

## 例: リクエスト情報

同じ要領で `Widget` 内でリクエスト情報を取得することもできます。以下は GET パラメータに応じてリストのソート順を決定する例です。

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.List (sortOn)
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
                Just "name" -> sortOn personName people
                Just "age"  -> sortOn personAge  people
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

今回の例では `handlerToWidget` すら呼び出す必要がないことに注意してください。その理由は、Yesod に含まれる多くの関数が `MonadHandler` 型クラスによって `Handler` と `Widget` の両方で自動的に動作するためです。実際に、`MonadHandler` は多くの共通のモナドトランスフォーマーを経由して、これらの関数を "自動的に持ち上げる" ことができます。

しかし、やりたいのであれば `runInputGet` の呼び出しを `handlerToWidget` でラップすることもできます。その場合、動作は全く同じです。

## パフォーマンスとエラーメッセージ

<div class="yesod-book-notice">
この節は読み飛ばしてもらっても構いません。ここからは Yesod の背後にある設計動機についての話になりますが、Yesod を使う上で必要不可欠というわけではないためです。
</div>

ここで、もしかすると少し混乱するかもしれません。既にお話したとおり、`Widget` シノニムはベースモナドに `Handler` ではなく `IO` を利用しています。そうなると、どうやって `Widget` は `Handler` のアクションを実行するのでしょうか？また、どうして `Widget` を `Handler` の上のトランスフォーマーとして構築し、`handlerToWidget` 関数の代わりに `lift` 関数を使うようにしないのでしょうか？さらには、`Widget` と `Handler` はどちらも `MonadResource` のインスタンスだと説明しました。`MonadResource` を使ったことがある人であれば、なぜ `ResourceT` がモナドトランスフォーマースタックに現れないのか不思議に思うかも知しれません。

この問題の真相としては、これらすべてのモナドトランスフォーマーに対応するためのずっと簡単な (実装面において) 方法があるということです。`Handler` は単なる `IO` ではなく `ResourceT IO` の上に構築されるトランスフォーマーということにしましょう。その方が少しだけ正確になりますからね。そして、`Widget` を `Handler` の上に構築されるモナドトランスフォーマーとしましょう。最終的には以下のようなコードになりますね。

```haskell
type Handler = HandlerT App (ResourceT IO)
type Widget  = WidgetT  App (HandlerT App (ResourceT IO))
```

この定義はそんなに悪く無さそうです。なぜなら、トランスフォーマー型を直接使わなくても、多くの部分で型シノニムを利用できるからです。問題点は、下地のトランスフォーマーがリークするするたびに、巨大な型シノニムは信じられないほどの混乱を招くということです。リークが最も起こりやすい場面はエラーメッセージです。おそらく、めちゃめちゃ混乱するでしょう！(他には、サブサイトで作業をする場合も同様に混乱するでしょう)

もう1つ心配なのは、モナドトランスフォーマーの層を追加するとパフォーマンスが悪化するということです。動作しているI/Oと比較すれば無視できるほどではありますが、オーバーヘッドはあります。

そこで、正しく層となっているトランスフォーマーの代わりに、`HandlerT` と `WidgetT` をそれぞれ1つのレベルのトランスフォーマーに平坦化します。このアプローチを抽象的な視点で見れば

- `HandlerT` は実際には `ReaderT` モナドです (エラーメッセージを明確にするために異なる名称にしただけです)。これは、リクエスト情報や、その他の不変コンテンツを含むような `HandlerData` 型を読み取り専用情報をとして持ちます。
- さらに、`HandlerData` は `GHState` (歴史的な理由でよくない名前が付いています) に対する `IORef` を持ち、それはハンドラ (例えば、セッション変数) の過程で変化するデータを保持します。`StateT` の代わりに `IORef` を利用するのは `IORef` は実行時例外が投げられたとしても、変化した状態を持ち続けるためです。
- `ResourceT` モナドトランスフォーマーは本質的には `ReaderT` に `IORef` を持たせたものです。この `IORef` は、実行しなければならないすべてのクリーンアップ処理に関する情報を含んでいます (これを`InternalState` と呼びます)。参照を保持する別々のトランスフォーマーの層を持つ代わりに `HandlerData` の自己参照を行います (当然、ここで `IORef` を採用する理由は、実行時例外のためでもあります)。
- `WidgetT` は本質的には `HandlerT` 上の `WriterT` ですが、`HandlerT` はただの `ReaderT` なので、2つの側面を1つのトランスフォーマーに簡単に圧縮できます。その結果 `newtype WidgetT site m a = WidgetT (HandlerData → m (a, WidgetData))` のようになります。

この部分をもっと理解したい場合は `Yesod.Core.Type` の `HandlerT` と `WidgetT` の定義を参照すると良いでしょう。

## 新しいモナドトランスフォーマーを追加する

自分のアプリケーションの一部に独自のモナドトランスフォーマーを追加したい時があるかもしれません。そういった時のために、例として Hackage にアップロードされている [monadcryptorandom](https://www.stackage.org/package/monadcryptorandom) パッケージを考えてみましょう。このパッケージはモナドに対して、暗号論的に安全な乱数を生成する `MonadCRandom` 型クラスと、その型クラスの具体的なインスタンスとして `CRandT` を定義しています。以下の例は、ランダムなバイト列を生成するコードです。

```haskell
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

しかし、結果は次のようなエラーメッセージが表示されます。

```shell
    No instance for (MonadCRandom e0 (HandlerT App IO))
      arising from a use of ‘getBytes’
    In a stmt of a 'do' block: randomBS <- getBytes 128
```

エラーメッセージに表示されるようなインスタンスをどのように取得すれば良いのでしょうか？ひとつの方法として `getBytes` を呼ぶ際に単純に `CRandT` モナドトランスフォーマーを使う方法があります。以下は完全な例です。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
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

この例で行っていることは `CRandT` トランスフォーマーを `HandlerT` トランスフォーマーの上に重ねています。他の方法では機能しません。Yesod 自身は、最終的に `CRandT` トランスフォーマーをアンラップする必要がありますが、そのための情報が欠如しているためです。このやり方は Persistent の時と同じように `HandlerT` の上に構築されるトランスフォーマーになります。

しかし、この方法には2つの短所があります。

1. ランダムな値を扱うごとに、この代わりのモナドに入り込む必要があります。
2. このやり方は非効率的です。他のモナドに入るたびに、新しいランダムシードを生成する必要があります。

2つ目の点については、ランダムなシードを `IORef` のような可変参照でファウンデーションデータ型に保存し、`CRandT` トランスフォーマーに入るたびに、アトミックにサンプリングすることで回避できます。しかし、さらにワンステップ深入りし `Handler` モナド自身を `MonadCRandom` のインスタンスにしてみましょう！コードは以下の通りです。

```haskell
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
import           Control.Monad              (join)
import           Control.Monad.Catch        (throwM)
import           Control.Monad.CryptoRandom
import           Control.Monad.Error.Class  (MonadError (..))
import           Crypto.Random              (SystemRandom, newGenIO)
import           Data.ByteString.Base16     (encode)
import           Data.IORef
import           Data.Text.Encoding         (decodeUtf8)
import           UnliftIO.Exception         (catch)
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
    getCRandom = wrap crandom
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

この例でもっとも大切な概念はこれらです。

1. `App` データ型が `IORef SystemRandom` のフィールドを持つように変更します。
2. 同様に `main` 関数で `IORef SystemRandom` の値を生成するように変更します。
3. `getHomeR` 関数はとても単純になります。トランスフォーマーを使わずに `getBytes` を呼ぶだけで良くなりました。
4. しかし、`MonadCRandom` インスタンス定義で複雑さが増加しました。本書は `monadcryptorandom` ではなく Yesod についての本なので、このインスタンスについての詳細には触れませんが、中身を調査し、もし興味があれば `CRandT` のインスタンスと比較することを推奨します。

幸いなことに、これは `HandlerT` トランスフォーマーの力という重要な点を理解するのに役立ちます。単に、読み取り可能な環境を与え、可変参照に頼ることで、`StateT` トランスフォーマーを再作成できます。実際に、もし実行時例外のために基底にある `IO` モナドを使っている場合、この抽象化によって `ReaderT`、`WriterT`、`StateT`、`ErrorT` の大部分を実装できます。

## まとめ

例え、この章を完全に無視したとしても `Yesod` を非常にうまく使うことができます。Yesod のモナドたちがどのようにして相互作用するかを理解することの利点は、より綺麗で柔軟性のあるコードを生成できる点にあります。任意のアクションを `Widget` で実行できることは、強力なツールになりますし、Persistent と `Handler` のコード間の相互作用を理解することは、アプリケーションをより適切に設計するために役立ちます。