---
title: 'Blog: i18n, authentication, authorization, and database'
published: 2021/02/28
---

# Blog: i18n, authentication, authorization, and database

これは単純なブログアプリである. 管理者がリッチテキストエディタ(nicedit)を用いてブログポストを追加したり, ログインしたユーザがコメントしたりすることが可能で, 完全にi18nをサポートしている. これはまた, Persistentデータベースを用いたり, Yesodの認可システムやテンプレートを活用する良い例である.

一般的にはテンプレート, Persistエンティティの定義, そして, ルーティングは別々のファイルに置くことを推奨しているが, ここでは利便性のため全てを1つのファイルにまとめている. 下の例で見る1つの例外はi18nメッセージである. 

まず言語拡張から始める. scaffoldedコードにおいては, 言語拡張はcabalファイルで指定されるため, 以下を個々のHaskellファイルに書く必要はない. 

``` haskell
{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
```

インポートは以下のようになる.

``` haskell
import Yesod
import Yesod.Auth
import Yesod.Form.Nic (YesodNic, nicHtmlField)
import Yesod.Auth.OpenId (IdentifierType(..), authOpenId)
import Data.Text (Text)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Conduit (Manager, newManager)
import Database.Persist.Sqlite
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration
    , createSqlitePool, runSqlPersistMPool
    )
import Data.Time (UTCTime, getCurrentTime)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Typeable (Typeable)
import Control.Monad.Logger (runStdoutLoggingT)
```

最初にPersistentエンティティをセットアップする. データ型(mkPersistを用いて)とmigration関数の両方を作るが, これにより自動的にSQLスキーマが作られ更新される. もしMongoDBバックエンドを用いていれば, migrationは不要である. 

``` haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
```

ユーザを追跡する. より堅牢なアプリケーションでは, アカウント作成日時, 表示名などを保存します.

``` haskell
User
   email Text
   UniqueUser email
```

Yesod-authのキャッシングと連携するために, `User`型は`Typable`のインスタンスである必要がある. 

``` haskell
deriving Typeable
```

個々のブログエントリ(リクエストメソッドのPOSTとの混乱のため, "ポスト"という言葉を使うのを避けた).

``` haskell
Entry
   title Text
   posted UTCTime
   content Html
```

次にブログポストのコメント.


``` haskell
Comment
   entry EntryId
   posted UTCTime
   user UserId
   name Text
   text Textarea
|]
```

各サイトはファウンデーションデータ型を持つ. この値はアプリケーションを立ち上げる前に初期化され, 全体で利用される. データベースのコネクションプールとHTTPコネクションマネージャを格納する. このファイルの最後の部分に, それらがどのように初期化されるかが示されている. 

``` haskell
data Blog = Blog
   { connPool    :: ConnectionPool
   , httpManager :: Manager
   }
```

i18nを容易で翻訳しやすくするために, 翻訳されるメッセージのための特別なファイル形式を用意する. 各言語ごとに1つのファイルが存在し, それぞれのファイルは言語コード(例えば, en, es, de-DE)に基づき命名され, フォルダに格納される. メイン言語ファイル(ここでは"en")も指定する. 

``` haskell
mkMessage "Blog" "blog-messages" "en"
```

`blog-messages/en.msg`は次のコンテンツを含んでいる. 

``` haskell
-- @blog-messages/en.msg
NotAnAdmin: You must be an administrator to access this page.

WelcomeHomepage: Welcome to the homepage
SeeArchive: See the archive

NoEntries: There are no entries in the blog
LoginToPost: Admins can login to post
NewEntry: Post to blog
NewEntryTitle: Title
NewEntryContent: Content

PleaseCorrectEntry: Your submitted entry had some errors, please correct and try again.
EntryCreated title@Text: Your new blog post, #{title}, has been created

EntryTitle title@Text: Blog post: #{title}
CommentsHeading: Comments
NoComments: There are no comments
AddCommentHeading: Add a Comment
LoginToComment: You must be logged in to comment
AddCommentButton: Add comment

CommentName: Your display name
CommentText: Comment
CommentAdded: Your comment has been added
PleaseCorrectComment: Your submitted comment had some errors, please correct and try again.

HomepageTitle: Yesod Blog Demo
BlogArchiveTitle: Blog Archive
```

さあ, ルーティングテーブルをセットアップしよう. 4つのエントリがある: ホームページ, エントリリストページ(`BlogR`), 個々のエントリページ(`EntryR`), そして, 認証サブサイト. `BlogR`と`EntryR`の両者はGETとPOSTメソッドを許容することに注意せよ. POSTメソッドはそれぞれ新しいブログを追加したり, 新しいコメントを追加したりするためにある.

``` haskell
mkYesod "Blog" [parseRoutes|
/              HomeR  GET
/blog          BlogR  GET POST
/blog/#EntryId EntryR GET POST
/auth          AuthR  Auth getAuth
|]
```

各ファウンデーションはYesod型クラスのインスタンスである必要がある. これは様々な設定を構成する場所である. 

``` haskell
instance Yesod Blog where
```

アプリケーションの最大の利点. BrowserIDが適切に機能するようにするために, これは有効なURLである必要がある.

``` haskell
approot = ApprootStatic "http://localhost:3000"
```

認可スキーム. 次のような規則を持ちたい.

- 管理者のみが新しいエントリを追加できる.

- ログインしたたユーザのみが新しいコメントを追加できる. 

- 他の全てのページは誰でもアクセスできる. 

ルートをRESTfulな方法でセットアップする. ここでは, 変更を加える操作は常に`POST`メソッドを用いている. その結果, 2つめのフィールドの`True`により, リクエストがwriteリクエストかどうかを簡単に確認できる. 

まず, 新しいエントリを追加するために, リクエストを認可する.

``` haskell
    isAuthorized BlogR True = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just (Entity _ user)
                | isAdmin user -> return Authorized
                | otherwise    -> unauthorizedI MsgNotAnAdmin
```

また, , 新しいコメントを追加するために, リクエストを認可する.

``` haskell
    isAuthorized (EntryR _) True = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just _  -> return Authorized
```

そして他の全てのリクエストでは, 結果は常に認可される.

``` haskell
    isAuthorized _ _ = return Authorized
```
もしユーザがAuthenticationRequiredを受け取った場合, リダイレクトされる.

``` haskell
    authRoute _ = Just (AuthR LoginR)
```

これはサイトの見た感じを定義する場所である. 関数は個々のページのコンテンツを与えられ, 標準テンプレートによりラップされる.

``` haskell
    defaultLayout inside = do
```

Yesodはget-following-postパターンを推奨し, ここではPOSTの後に他のページにリダイレクトされる. POSTページがユーザにフィードバックを与えられるようにするために, `getMessage`や`setMessage`関数がある. defaultLayout関数で常にペンディングのメッセージを確認することはよいアイデアである.

``` haskell
mmsg <- getMessage
```

HTML, CSS, そしてJavascriptを共に合成するために, widgetを用います. 最終的には, それら全てをアンラップし, 単純なHTMLにする必要がある. そのために`widgetToPageContent`が存在する. それに対し, 個々のページ(内部の)から受け取ったコンテンツと, 全てのページに対する標準CSSから成るwidgetを与える. Luciusテンプレート言語を用いて後者を作る. 

``` haskell
        pc <- widgetToPageContent $ do
            toWidget [lucius|
body {
    width: 760px;
    margin: 1em auto;
    font-family: sans-serif;
}
textarea {
    width: 400px;
    height: 200px;
}
#message {
  color: #900;
}
|]
            inside
```

そして, 最終的に新しいHamletテンプレートを用いて, 個々のコンポーネント(タイトル, ヘッドデータ, そしてボディデータ)をラップし最終的な出力に変換する. 

``` haskell
        withUrlRenderer [hamlet|
$doctype 5
<html>
    <head>
        <title>#{pageTitle pc}
        ^{pageHead pc}
    <body>
        $maybe msg <- mmsg
            <div #message>#{msg}
        ^{pageBody pc}
|]
```

これはユーザが管理者かどうかを確認するための単純な関数である. 実際のアプリケーションにおいては, 管理者のビットをデータベース自身に格納するか, 何らかの外部ファイルで確認する傾向がある. 今回は, 自分のeメールアドレスを単にハードコードした. 

``` haskell
isAdmin :: User -> Bool
isAdmin user = userEmail user == "michael@snoyman.com"
```

データベースにアクセスするために, YesodPersistインスタンスを作る必要があり, そこではどのバックエンドを使い, どのようにアクションを実行するかを宣言する. 

``` haskell
instance YesodPersist Blog where
   type YesodPersistBackend Blog = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
```

これは利便性のための同義語であり, scaffoldedサイトで自動的に定義される.

``` haskell
type Form x = Html -> MForm Handler (FormResult x, Widget)
```

yesod-formとyesod-authを使うために, FormMessageのためにRenderMessageのインスタンスが必要になる. これにより, 個々のフォームメッセージにおけるi18nを制御することが可能になる.

``` haskell
instance RenderMessage Blog FormMessage where
    renderMessage _ _ = defaultFormMessage
```

組み込みのnic HTMLエディタを用いるために, このインスタンスが必要になる. デフォルト値だけを取ればよく, これはNicのCDNでホストされたバージョンを用いる. 

``` haskell
instance YesodNic Blog
```

yesod-authを用いるために, YesodAuthインスタンスが必要である.

``` haskell
instance YesodAuth Blog where
    type AuthId Blog = UserId
    loginDest _ = HomeR
    logoutDest _ = HomeR
```

外部のOpenIDプロバイダを用いてユーザを認証し, eメールアドレスをユーザidとして用いるようにリクエストする. これにより将来, 局所的に認証されたeメールアドレス(これもまたyesod-authに含まれている)のように, 他システムに切り替えることが容易になる.

この関数はログインクレデンシャル(eメールアドレスを含む)を取り, UserIdを返す.

``` haskell
    getAuthId creds =
      -- Key name for email value may vary between providers
      let emailKey = "openid.ax.value.email" in
      case lookup emailKey (credsExtra creds) of
          Just email -> do
              res <- liftHandler $ runDB $ insertBy (User email)
              return $ Just $ either entityKey id res
          Nothing -> return Nothing
```

また, Persistentと連携するために`YesodAuthPersist`インスタンスを与える必要がある.

``` haskell
instance YesodAuthPersist Blog

```

ホームページのハンドラ. ここでの重要な詳細は`setTitleI`の使い方であり, i18nメッセージをタイトルに用いることが可能になる. またこのメッセージをHamletにおいて`_{Msg...}`展開で用いる.

``` haskell
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitleI MsgHomepageTitle
    [whamlet|
<p>_{MsgWelcomeHomepage}
<p>
   <a href=@{BlogR}>_{MsgSeeArchive}
|]
```

新しいエントリを追加するためにフォームを定義しよう. ユーザにタイトルとコンテンツを提供してもらい, ポスト日時を`getCurrentTime`を通し自動的に埋めてもらいたい.

少し奇妙な`lift (liftIO getCurrentTime)`による`IO`アクションを実行する方法に注意せよ. 理由としてはapplicative formはモナドでないためであり, そのため`MonadIO`のインスタンスになれないためである. 代わりに, `lift`を用いて基盤にある`Handler`モナドでアクションを実行し, `liftIO`を用いて`IO`アクションを`Handler`アクションに変換する.

``` haskell
entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField (fieldSettingsLabel MsgNewEntryTitle) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> areq nicHtmlField (fieldSettingsLabel MsgNewEntryContent) Nothing
```

ブログエントリのリストを取得し, 管理者にフォームで提出し, 新しいエントリを作る.

``` haskell
getBlogR :: Handler Html
getBlogR = do
    muser <- maybeAuth
    entries <- runDB $ selectList [] [Desc EntryPosted]
    (entryWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        setTitleI MsgBlogArchiveTitle
        [whamlet|
$if null entries
    <p>_{MsgNoEntries}
$else
    <ul>
        $forall Entity entryId entry <- entries
            <li>
                <a href=@{EntryR entryId}>#{entryTitle entry}
```

3通りの可能性がある: ユーザが管理者としてログインしている, ユーザがログインしているが管理者ではない, ユーザはログインしていない. 最初の場合, エントリフォームを表示すべきである. 2つ目の場合, 何もしない. 3つ目の場合, ログインリンクを与える.

``` haskell
$maybe Entity _ user <- muser
    $if isAdmin user
        <form method=post enctype=#{enctype}>
            ^{entryWidget}
            <div>
                <input type=submit value=_{MsgNewEntry}>
$nothing
    <p>
        <a href=@{AuthR LoginR}>_{MsgLoginToPost}
|]
```

入ってくるエントリ追加の処理をする. パーミッションの確認は全く行わなくてよい, なぜならば`isAuthorized`がそれを処理するためである. もしフォームの提出が有効であれば, エントリをデータベースに追加し, 新しいエントリにリダイレクトする. そうでなければ, ユーザに再度行うように求める. 

``` haskell
postBlogR :: Handler Html
postBlogR = do
    ((res, entryWidget), enctype) <- runFormPost entryForm
    case res of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            setMessageI $ MsgEntryCreated $ entryTitle entry
            redirect $ EntryR entryId
        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectEntry
            [whamlet|
<form method=post enctype=#{enctype}>
    ^{entryWidget}
    <div>
        <input type=submit value=_{MsgNewEntry}>
|]
```

コメントのためのフォーム, 上の`entryForm`とかなり類似している. これはコメントが紐づいているエントリの`EntryId`を取る. pureを用いることで, この値を結果のComment出力に埋め込み, 生成されたHTMLに現れないようにする. 

``` haskell
commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
    <$> pure entryId
    <*> lift (liftIO getCurrentTime)
    <*> lift requireAuthId
    <*> areq textField (fieldSettingsLabel MsgCommentName) Nothing
    <*> areq textareaField (fieldSettingsLabel MsgCommentText) Nothing
```

個々のエントリ, コメントを示し, ユーザがログインしていればコメントフォームを追加する.  

``` haskell
getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
    (entry, comments) <- runDB $ do
        entry <- get404 entryId
        comments <- selectList [CommentEntry ==. entryId] [Asc CommentPosted]
        return (entry, map entityVal comments)
    muser <- maybeAuth
    (commentWidget, enctype) <-
        generateFormPost (commentForm entryId)
    defaultLayout $ do
        setTitleI $ MsgEntryTitle $ entryTitle entry
        [whamlet|
<h1>#{entryTitle entry}
<article>#{entryContent entry}
    <section .comments>
        <h1>_{MsgCommentsHeading}
        $if null comments
            <p>_{MsgNoComments}
        $else
            $forall Comment _entry posted _user name text <- comments
                <div .comment>
                    <span .by>#{name}
                    <span .at>#{show posted}
                    <div .content>#{text}
        <section>
            <h1>_{MsgAddCommentHeading}
            $maybe _ <- muser
                <form method=post enctype=#{enctype}>
                    ^{commentWidget}
                    <div>
                        <input type=submit value=_{MsgAddCommentButton}>
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>_{MsgLoginToComment}
|]
```

入ってくるコメント提出を受け取る.

``` haskell
postEntryR :: EntryId -> Handler Html
postEntryR entryId = do
    ((res, commentWidget), enctype) <-
        runFormPost (commentForm entryId)
    case res of
        FormSuccess comment -> do
            _ <- runDB $ insert comment
            setMessageI MsgCommentAdded
            redirect $ EntryR entryId
        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectComment
            [whamlet|
<form method=post enctype=#{enctype}>
    ^{commentWidget}
    <div>
        <input type=submit value=_{MsgAddCommentButton}>
|]
```

最後にmain関数を書く.

``` haskell
main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "blog.db3" 10 -- create a new pool
    -- perform any necessary migration
    runSqlPersistMPool (runMigration migrateAll) pool
    manager <- newManager tlsManagerSettings -- create a new HTTP manager
    warp 3000 $ Blog pool manager -- start our server
```