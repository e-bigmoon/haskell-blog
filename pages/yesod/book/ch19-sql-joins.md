---
title: SQL Joins
published: 2020/03/20
# updated: 2018/04/07
---

Persistent の特徴はデータベースに依存しないインターフェースです。それでは、どのようにして本来バックエンド特有の処理を行うのでしょうか？この問題は Yesod で2つのテーブルを結合する場合などで発生します。完全にバックエンドに依存しない純粋な Haskell による解決方法もありますが、それよりも使いやすい効率的な方法があります。この章では、まずはこれらの良くある問題と解決方法について紹介し、その後、より高度な解決策について学びます。

## 複数の著者によるブログ

ブログはよく理解された問題領域のため、説明のための題材として利用します。データベースに複数の著者を登録することができるブログエンジンを考えましょう。ただし、それぞれのブログ記事には1人の著者がいるということにします。Persistent では次のようにモデル化します。

```haskell
Author
    name Text
Blog
    author AuthorId
    title Text
    content Html
```

ブログタイトルと著者を紐付けるブログ記事インデックスを表示するための Yesod アプリケーション (第1バージョン) をセットアップしましょう。

```haskell
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

とりあえず、この内容で出力を確認してみましょう。

![著者を数字の識別子として表示](https://www.yesodweb.com/book/image/blog-bad-author)

ここでは、著者名の代わりに各著者の数字の識別子を表示しているだけです。これを修正するためには `Author` テーブルから追加の情報を引き出す必要があります。それを行ってみましょう。

## ウィジェット内のデータベースクエリ

これについてはおそらく多くのユーザを驚かせてしまうので、すぐに説明することにします。この問題は Hamlet テンプレートで解決できると思うかもしれません。例えば以下のようにです。

```haskell
<ul>
    $forall Entity blogid blog <- blogs
        $with author <- runDB $ get404 $ blogAuthor
            <li>
                <a href=@{BlogR blogid}>
                    #{blogTitle blog} by #{authorName author}
``` 

上記のコードは Hamlet の中でデータベースアクションを実行することができないため、コンパイルできません。Shakespeare テンプレートのゴールの1つは純粋コードと非純粋コードを分離することです。そうすることで、すべての副作用を持つコードを Hasekll に留めておけます。

ただ、コードを少しいじれば Yesod で動かせます。アイデアとしては各ブログエントリのコードを `Widget` 関数に分離し、Haskell の場所でデータベースアクションを実行します。

```haskell
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

`Handler` アクションを `Widget` アクションに変換するために `handlerToWidget` を使う必要がありますが、コードは簡単です。これで期待通りの結果が得られます。

![著者を名前で表示](https://www.yesodweb.com/book/image/blog-good-author)

## Joins

ここまでで、求めていた結果が得られたはずなのに、なぜこの章はまだ続いているのでしょうか？問題はこの技術が非常に非効率的だという点です。データベースクエリを1つ実行するだけで、全てブログ記事が読み込まれ、それぞれのブログ記事ごとに著者名を取得するための別のクエリが実行されます。これは単純に SQL join を使う場合と比べて、かなり非効率的です。ここでの疑問は Persistent で join を行うためにはどうすれば良いか？ということです。まずは適当な raw SQL を書くことから始めましょう。

```haskell
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

`rawSql` 関数には、SQL クエリとクエリ内のプレースホルダを置換する追加のパラメータのリストを引数として渡します。ここではプレースホルダを利用していないため、このリストは空です。しかし、`SELECT` 文で `??` を使っている点に注目してください。これは型検査の形式です。`rawSql` は要求されているエンティティの型を見つけ、自動的にクエリを作るために必要なフィールドを埋めます。

`rawSql` は確かに強力ですが、安全ではありません。SQLクエリ文字列の構文チェックを行わないため、実行時エラーが発生するかもしれません。また、簡単に誤った型のクエリを作成できるため、それによってかなり混乱する実行時エラーメッセージが表示されることもあります。

## Esqueleto

<div class="yesod-book-notice">
現在、最新の LTS Haskell には Esqueleto は含まれていません。そのため Eqsueleto を利用するためには少し作業が必要となるかもしれませんが、本書ではカバーしません。
</div>

Persistent の仲間に Esqueleto と呼ばれるライブラリがあります。このライブラリは SQL クエリを記述するための表現力豊かな型安全DSLを提供します。Esqueleto は Persistent の型の強みを利用して有効な SQL クエリの生成を保証し、プログラムによって要求された結果を生成します。Esqueleto を使うためにいくつかインポートを追加しましょう。

```haskell
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
``` 

次に Esqueleto を使ってクエリを書きます。

```haskell
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

このクエリは私たちが先ほど書いた SQL にとても類似しているという点に着目してください。特に興味深いのは `^.` 演算子です。これは射影 (*projection*) と呼ばれます。例えば `blog ^. BlogAuthor` は "`blog` テーブルの `author` カラムを取得する" ことを意味します。さらに、Esqueleto の型安全性のおかげで `blog` から `AuthorName` を射影してしまうようなアクシデントは絶対に発生しません。型システムがこれを拒みます！

Esqueleto は安全性の他にも性能の点で優れています。`return` されるタプルを見てください。ここでは、箇条書きを生成するために必要な3つのカラムを明示的に並べています。これがパフォーマンス上の非常に大きな利点となります。これまでの全ての例と違って、箇条書きを生成するためにブログ記事の (おそらく非常に大きな) `content` カラムを転送する必要はありません。

<div class="yesod-book-notice">
一応、`rawSql` を使って同じことを実現できますが、少し技巧的です。 
</div>

実際のところ Esqueleto は Persistent で SQL クエリを書くための非常に優れた標準的方法です。経験則としては、もし自然に Persistent のクエリ構文に適合するのであれあば Persistent を使います。なぜなら、データベースに依存せず、利用方法も Esqueleto と比べて少しだけ簡単だからです。しかし、もし SQL 独自の機能を利用する効率化に取り組んでいる場合、Esqueleto の利用を真剣に考えるべきです。

## ストリーミング

Esqueleto による方法にはまだ問題があります。もし、数千ものブログ記事が存在する場合、処理の流れは以下のようになります。

1. サーバのメモリに数千ものブログ記事を読み込む
1. HTML ページ全体をレンダリングする
1. HTML ページをクライアントに送る

これは2つの欠点があります。それは大量のメモリを消費し、ユーザに対してかなりの待ち時間が発生します。しかし、これが悪い方法ということであれば、なぜ Yesod はストリーミングの方法の代わりにこの方法を Yesod の標準として組み込んでいるのでしょうか？これには2つの理由があります。

- 正確さ: データベースから243番目のレコードを読み込む際にエラーが発生したとしましょう。ノンストリーミングなレスポンスを行うことで Yesod は例外をキャッチし、意味のある500エラーレスポンスを送ることができます。もし、すでにストリーミングを行なっていればストリーミング本体は誤解の恐れのある200 OKレスポンスの途中で止まってしまいます。
- 使いやすさ: 多くの場合、ノンストリーミングで作業する方が簡単です。

巨大になってしまうかもしれない一覧表を生成したいユーザに対する標準的な推奨方法としては、ページネーションを利用することです。ページネーションはサーバ側の作業を減らし、シンプルなコードを書き、Yesod が提供する正確性を保証し、ユーザの待機時間を減らすことができます。しかしながら、本当にストリーミングレスポンスを行いたい時もあるので、ここで説明することにしましょう。

Esqueleto をストリーミングレスポンスに切り替えることは簡単です。`select` を `selectSource` で置き換えるだけです。Esqueleto のクエリ自身を変更する必要はありません。そして `respondSourceDB` 関数を使ってストリーミングデータベースレスポンスを生成し、一覧表を仕上げるために手動で HTML を書きます。

```haskell
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

`sendChunkText` の使い方に注目してください。ここでネットワーク上に生の `Text` 値を送っています。その後、各ブログタプルを取り、conduit の `map` 関数を使ってストリーミング値を作ります。私たちは `hamlet` でテンプレートを取得した後、型安全URLをテキスト形式のURLに変換するために `render` 関数を渡します。最後に `toFlushBuilder` は `Html` の値を Yesod のストリーミングフレームワークが必要とする `Flush Builder` の値に変換します。

残念なことに、これらの変更によって Hamlet を利用したページ全体のレイアウト生成ができなくなりました。なぜなら、開始タグと終了タグを別々に明示的に生成する必要があるためです。もし偶然、間違ってアンバランスなタグを作ってしまった場合、これにより別のバグが発生する可能性があります。また、 `defaultLayout` を利用する能力も同じ理由によって失われてしまいます。

ストリーミングHTMLレスポンスは強力なツールなので時々必要になりますが、一般的には安全なオプションを推奨します。

## 結論

この章は SQL join を行うためのいくつもの方法を扱いました。

- join を完全に避けて、Haskell で関連するデータを手動で操作する方法。これはアプリケーションレベルの join としても知られています。
- `rawSql` を使って SQL を明示的に書く方法。ある程度は便利ですが、Persistent の型安全性の多くを犠牲にします。
- Esqueleto の DSL 機能によって、型安全SQLクエリを作る方法。
- 必要に応じて Esqueleto からストリーミングレスポンスを生成することもできます。

完全性のため、ここに最後の完全なストリーミングレスポンスの例を示します。

```haskell
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

## 本書のコード

- [Example01.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch19/Example1.hs)
- [Example02.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch19/Example2.hs)
- [Example03.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch19/Example3.hs)
- [Example04.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch19/Example4.hs)
- [Example05.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch19/Example5.hs)
- [Example06.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch19/Example6.hs)