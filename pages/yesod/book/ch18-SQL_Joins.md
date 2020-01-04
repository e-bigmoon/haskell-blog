---
title: SQL Joins
date: 2019/01/20
---

パージステントはデータベースに依存しないインターフェースとして自身を売り込む. それでは, どのようにして本来バックエンド特有の事を行うのであろうか? これはYesodにおいて, 2つのテーブルを結合したい場合にもっとも頻繁に生じる. 完全にバックエンドに依存しない純粋なHaskellの解決策が存在するが, 自在になるより効率的な方法が存在する. この章においては, 解決したいであろう一般的な問題について紹介し, より洗練された方法を構築する. 

## 複数の著者によるブログ

ブログはよく理解された問題領域のため, 問題設定のためにそれを用いる. データベースに複数の著者を持つことを許容するブログエンジンを考えよう. また, 各ブログポストには単一の著者がいる. パージステントでは, これを次のようにモデル化する:

``` haskell
Author
    name Text
Blog
    author AuthorId
    title Text
    content Html
```

ブログタイトルと著者を示すブログポストのインデックスを見せるための, 最初のYesodアプリケーションをセットアップしましょう:

``` haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import           Control.Monad.Logger
import           Data.Text               (Text)
import           Database.Persist.Sqlite
import           Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author
    name Text
Blog
    author AuthorId
    title Text
    content Html
|]

data App = App
    { persistConfig :: SqliteConf
    , connPool      :: ConnectionPool
    }
instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

mkYesod "App" [parseRoutes|
/ HomeR GET
/blog/#BlogId BlogR GET
|]

getHomeR :: Handler Html
getHomeR = do
    blogs <- runDB $ selectList [] []

    defaultLayout $ do
        setTitle "Blog posts"
        [whamlet|
            <ul>
                $forall Entity blogid blog <- blogs
                    <li>
                        <a href=@{BlogR blogid}>
                            #{blogTitle blog} by #{show $ blogAuthor blog}
        |]

getBlogR :: BlogId -> Handler Html
getBlogR _ = error "Implementation left as exercise to reader"

main :: IO ()
main = do
    -- Use an in-memory database with 1 connection. Terrible for production,
    -- but useful for testing.
    let conf = SqliteConf ":memory:" 1
    pool <- createPoolConfig conf
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        -- Fill in some testing data
        alice <- insert $ Author "Alice"
        bob   <- insert $ Author "Bob"

        insert_ $ Blog alice "Alice's first post" "Hello World!"
        insert_ $ Blog bob "Bob's first post" "Hello World!!!"
        insert_ $ Blog alice "Alice's second post" "Goodbye World!"

    warp 3000 App
        { persistConfig = conf
        , connPool      = pool
        }
```

これですべてうまくできますが, 出力を見てみましょう:

![image/blog-bad-author](https://www.yesodweb.com/book/image/blog-bad-author "image/blog-bad-author")
<img src="image/blog-bad-author">

Authors appear as numeric identifiers

ここで行なっていることはすべて, 名前の代わりに各著者の識別子を表示しているだけである. これを修繕するために, 同様に`Author`テーブルからさらなる情報を引っ張ってくる必要がある. それを行なってみましょう. 

## ウィジットにおけるデータベースクエリ

これについてはおそらく多くのユーザを驚かせるので, すぐに説明することにする. この問題はHamletテンプレートそれ自身で解決できると思うかもしれない, 例えば:

``` haskell
<ul>
    $forall Entity blogid blog <- blogs
        $with author <- runDB $ get404 $ blogAuthor
            <li>
                <a href=@{BlogR blogid}>
                    #{blogTitle blog} by #{authorName author}
``` 

しかし, これは許可されない. なぜならば, Hamletは中でデータベースアクションを実行することができないためである. Shakespeareテンプレートにおけるゴールの1つは, すべての非純粋コードをHasekllに残したまま, 純粋コードと非純粋コードを分離することである. 

しかし, 上のコードに手を加えてYesodにおいて機能するようにできる. アイデアとしては, 各ブログエントリにおけるコードを`Widget`関数に分離し, 関数のHaskell部分においてデータベースアクションを実行することである. 

``` haskell
getHomeR :: Handler Html
getHomeR = do
    blogs <- runDB $ selectList [] []

    defaultLayout $ do
        setTitle "Blog posts"
        [whamlet|
            <ul>
                $forall blogEntity <- blogs
                    ^{showBlogLink blogEntity}
        |]

showBlogLink :: Entity Blog -> Widget
showBlogLink (Entity blogid blog) = do
    author <- handlerToWidget $ runDB $ get404 $ blogAuthor blog
    [whamlet|
        <li>
            <a href=@{BlogR blogid}>
                #{blogTitle blog} by #{authorName author}
    |]


```

`Handler`アクションを`Widget`アクションに変換するために`handlerToWidget`を使う必要があるが, コードは率直である. さらに, 今や望んだ結果を得ることができる.

Authors appear as names
![image/blog-good-author](https://www.yesodweb.com/book/image/blog-good-author "image/blog-good-author")
<img src="image/blog-good-author">


## Joins

もし, まさに探している結果を得たのなら, なぜこの章は終了しないのであろうか? 問題はこの技術は非常に非効率的であるということである. 全てのブログポストを積み込むために1つのデータベースクエリを実行している. そして, 各々のブログポストにおいて著者の名前を得るために, 分けられたクエリを実行している. これは単にSQL joinを使うのに比べ, 非効率的である. 問題点は: どのようにPersistentにおいて, joinを行うかである. いくつかの生SQLを書くことで始める.

``` haskell

getHomeR :: Handler Html
getHomeR = do
    blogs <- runDB $ rawSql
        "SELECT ??, ?? \
        \FROM blog INNER JOIN author \
        \ON blog.author=author.id"
        []

    defaultLayout $ do
        setTitle "Blog posts"
        [whamlet|
            <ul>
                $forall (Entity blogid blog, Entity _ author) <- blogs
                    <li>
                        <a href=@{BlogR blogid}>
                            #{blogTitle blog} by #{authorName author}
        |]

```

`rawSql`関数に対し, 2つのパラメータを渡す: SQLクエリと, クエリにおけるプレースホルダに置き換える追加パラメータのリストである. プレースホルダは扱っていないため, このリストは空である. しかし, `SELECT`宣言において, `??`を用いていることに注意しなさい. これは型検査の1つの形式である: `rawSql`は要求されているエンティティの型を見つけ, 自動的にクエリを作るために必要なフィールドを埋める. 
`rawSql`は確かに強力であるが, 安全ではない. SQLクエリ文字列には, 構文チェックが存在せず, 実行時エラーが発生しうる. また, 容易に誤った型をクエリし, かなり混乱する実行時エラーメッセージが表示されることもある. 

## Esqueleto

<p class = "info">
現在, 最近のLTS HaskellにはEsqueletoは含まれていない. そのため, それを使うためには, この本で述べられていないような追加作業を少し行う必要があるかもしれない.
</p>

PersistentにはEsqueletoと呼ばれる対が存在し, SQLクエリを書くための表現的な, 型安全DSLを与える. それは, Persistentの型を利用し, 有効なSQLクエリを生成し, プログラムによりリクエストされた結果を生成することを保証する. Esqueletoを使うために, いくつかのインポートを追加する. 

``` haskell
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
``` 

そして, Esqueletoを用いてクエリを書く.

``` haskell
getHomeR :: Handler Html
getHomeR = do
    blogs <- runDB
           $ E.select
           $ E.from $ \(blog `E.InnerJoin` author) -> do
                E.on $ blog ^. BlogAuthor E.==. author ^. AuthorId
                return
                    ( blog   ^. BlogId
                    , blog   ^. BlogTitle
                    , author ^. AuthorName
                    )

    defaultLayout $ do
        setTitle "Blog posts"
        [whamlet|
            <ul>
                $forall (E.Value blogid, E.Value title, E.Value name) <- blogs
                    <li>
                        <a href=@{BlogR blogid}>#{title} by #{name}
        |]
```

クエリが先ほど書いたSQLに, いかに類似しているか留意しなさい. 特に興味深いのは, `^.`演算子であり, これは射影と呼ばれる. 例えば, `blog ^. BlogAuthor`は"`blog`テーブルの`author`カラムを取りなさい"を意味する. そして, 型安全性のため, 決して偶然的にでも`blog`から`AuthorName`を射影しないのである: 型システムはこれを拒む!

安全性の他に, Esqueletoにはパフォーマンス上の利点がある. `return`されるタプルを見なさい; それは, 箇条書きを生成するために必要なカラムを明示的に並べている. これはパフォーマンス上の大きな利点となる. これまでにあった例と違い, これは箇条書きを生成するために(おそらく非常に大きな)`content`カラムを転送する必要がない. 

<p class = "info">
実際には`rawsql`を用いて同様に達成することは可能であるが, 少し技巧的である. 
</p>

Esqueletoは実際にはPersistentにおいてSQLクエリを書くための標準である.  大まかなやりかたとしては: もし自然にPersistentのクエリ構文に自然に適合するようなことを行なっていれば, Persistentを使いなさい. なぜならば, それはデータベースに依存せず, 使うのが少し容易であるからである. しかし, もし, SQL独自の機能を使えばより効率的になるようなものに取り組んでいれば, Esqueletoを使うことを強く考えるべきである.

## Streaming

Esqueletoによる方法にはまだ問題がある. もし, 数千ものブログポストが存在する場合, ワークフローは以下のようになる:

1. サーバ上のメモリに数千ものブログポストを読み込む.

2. HTMLページ全体をレンダリングする.

3. HTMLページをクライアントに送る. 

これは2つの欠点がある: それは, 多くのメモリを消費し, ユーザに対し, かなりの待ち時間を要する. もし, これが悪い方法であれば, なぜYesodはストリーミングによる方法の代わりに, それをYesodの標準として組み込んでいるのでしょうか? これには, 2つの理由がある:

- 正確さ : データベースから243番目のレコードを読むのにエラーが生じたとしよう. ストリーミングによらないレスポンスを行うことで, Yesodは例外をキャッチし, 意味のある500エラーレスポンスを送ることができる. もしすでにストリーミングを行なっていれば, ストリーミング本体は誤解の恐れのある200 OK反応の途中で, 止まってしまうだけである. 

- 使いやすさ : たいていストリーミングのない本体で作業する方が容易である. 

膨大になるかもしれない一覧表を生成したい人に対する標準的な推奨としては, ページネーションを用いることである. これにより, サーバ側での作業を減らし, 単純なコードを書き, Yesodが提供する正しさの保証を得, ユーザの待機時間を減らすことができる. しかし, 実際にストリーミングレスポンスを行いたい時もあるため, ここではそれをカバーする. 

Esqueletoをストリーミングレスポンスに切り替えることは容易である: `select`を`selectSource`で置き換えよ. Esqueletoのクエリ自身は不変である. そして, `respondSourceDB`関数を用いて, ストリーミングデータベースレスポンスを生成し, 一覧表を仕上げるために手動でHTMLを構築する. 

``` haskell

getHomeR :: Handler TypedContent
getHomeR = do
    let blogsSrc =
             E.selectSource
           $ E.from $ \(blog `E.InnerJoin` author) -> do
                E.on $ blog ^. BlogAuthor E.==. author ^. AuthorId
                return
                    ( blog   ^. BlogId
                    , blog   ^. BlogTitle
                    , author ^. AuthorName
                    )

    render <- getUrlRenderParams
    respondSourceDB typeHtml $ do
        sendChunkText "<html><head><title>Blog posts</title></head><body><ul>"
        blogsSrc $= CL.map (\(E.Value blogid, E.Value title, E.Value name) ->
            toFlushBuilder $
            [hamlet|
                <li>
                    <a href=@{BlogR blogid}>#{title} by #{name}
            |] render
            )
        sendChunkText "</ul></body></html>"

```

`sendChunkText`の使い方に注目しなさい. これは, ネットワーク上に生の`Text`値を送る. その後, ブログにおけるタプルを取り, conduitの`map`関数を用いて, ストリーミング値を作る. `hamlet`を用いて, テンプレートを作り, `render`関数を渡し, 型安全URLをテキストに変換する. 最後に, `toFlushBuilder`は`Yesodのストリーミングフレームワークにおいて必要になるため, Html`値を`Flush Builder`値に変換する. 

残念なことに, もやはHamletを利用してページ全体のレイアウトを生成できない. なぜならば, 生成開始と終了タグを別々に明確にする必要があるためである. もし, 偶然, 誤ったタグを作ってしまった場合, これにより別のバグの可能性が生じてくる. また, `defaultLayout`を用いる能力も全く同じ理由により失うことになる. 

HTMLレスポンスのストリーミングは強力な武器であり, 時々必要になる. しかし, 一般的に, より安全な選択肢に固執するのを勧める. 

## 結論

この章はSQL joinを行うためのいくつもの方法を扱った: 

- joinを完全に避け, Haskellにおける関連するデータを手動で取ってくる. これはアプリケーションレベルでのjoinとしても知られている. 

- `rawSql`を用いてSQLを明示的に書く. 少し便利であるが, これはPersistentの型安全性の多くを犠牲にする.

- EsqueletoのDSL機能を用いて, 型安全SQLクエリを作りなさい.

- もし必要なら, Esqueletoからストリーミングレスポンスを生成することもできる. 

完全性のため, ここに最後の完全なストリーミングレスポンスの例を挙げる:

``` haskell

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import           Control.Monad.Logger
import           Data.Text               (Text)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Database.Persist.Sqlite
import           Yesod
import qualified Data.Conduit.List as CL
import Data.Conduit (($=))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author
    name Text
Blog
    author AuthorId
    title Text
    content Html
|]

data App = App
    { persistConfig :: SqliteConf
    , connPool      :: ConnectionPool
    }
instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

mkYesod "App" [parseRoutes|
/ HomeR GET
/blog/#BlogId BlogR GET
|]

getHomeR :: Handler TypedContent
getHomeR = do
    let blogsSrc =
             E.selectSource
           $ E.from $ \(blog `E.InnerJoin` author) -> do
                E.on $ blog ^. BlogAuthor E.==. author ^. AuthorId
                return
                    ( blog   ^. BlogId
                    , blog   ^. BlogTitle
                    , author ^. AuthorName
                    )

    render <- getUrlRenderParams
    respondSourceDB typeHtml $ do
        sendChunkText "<html><head><title>Blog posts</title></head><body><ul>"
        blogsSrc $= CL.map (\(E.Value blogid, E.Value title, E.Value name) ->
            toFlushBuilder $
            [hamlet|
                <li>
                    <a href=@{BlogR blogid}>#{title} by #{name}
            |] render
            )
        sendChunkText "</ul></body></html>"

getBlogR :: BlogId -> Handler Html
getBlogR _ = error "Implementation left as exercise to reader"

main :: IO ()
main = do
    -- Use an in-memory database with 1 connection. Terrible for production,
    -- but useful for testing.
    let conf = SqliteConf ":memory:" 1
    pool <- createPoolConfig conf
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        -- Fill in some testing data
        alice <- insert $ Author "Alice"
        bob   <- insert $ Author "Bob"

        insert_ $ Blog alice "Alice's first post" "Hello World!"
        insert_ $ Blog bob "Bob's first post" "Hello World!!!"
        insert_ $ Blog alice "Alice's second post" "Goodbye World!"

    warp 3000 App
        { persistConfig = conf
        , connPool      = pool
        }
        
```



