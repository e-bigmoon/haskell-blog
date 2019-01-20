---
title: 認証と認可
date: 2019/01/20
---

認証と認可はとても良く似ていますが、それぞれ異なる概念です。認証はユーザを識別し、認可はユーザの権限を決定します。残念なことに、これらの用語はどちらも "auth" と省略されるため、両者の概念を混同してしまう場合があります。

Yesod は OpenID、BrowserID や OAuth のような多くの第三者認証システムを組み込めるようになっています。つまり、これらの第三者認証システムを組み込むということは、アプリケーションはユーザの認証情報の検証に関して、外部のシステムを信頼すると言うことになります。また、良くある username/password や email/password を使った認証システムについても同様にサポートしています。第三者認証システムの方式ではユーザ (新しいパスワードを覚えずに済む) と実装者 (セキュリティアーキテクチャ全体を扱う必要がなくなる) にとって簡単にはなりますが、開発者が制御できる範囲という視点で考えれば自前の認証システムの方が柔軟です。

認可については REST と型安全URLの長所を活かすことで、シンプルかつ宣言的なシステムを作ることができます。さらに、認可に関するのコードは全て Haskell で記述されているため、ユーザは自分の好きなようにカスタマイズすることもできます。

この章では、Yesod で認証を扱う方法と、異なる認証システムの選択におけるトレードオフについて議論します。

## 概略

yesod-auth パッケージはいくつもの異なる認証プラグインのための共通インターフェースを提供します。これらのバックエンドには一意な文字列に基づいてユーザを識別することが求められます。例えば OpenID の場合、一意な文字列は実際の OpenID 値になるでしょう。 BrowserID では、メールアドレス、さらに HashDB (ハッシュ化されたパスワードのデータベースを用いる) では、ユーザ名に相当します。

各認証プラグインは、ログインするための独自システムを提供します。ログインシステムは外部サイトや email/password フォーム経由でトークンをやり取りします。ログインが成功すると、プラグインはユーザのセッションに値にユーザを表す `AuthId` を設定します。この`AuthId` は基本的には、ユーザを追跡するために用いられるテーブルからの Persistent ID です。

ユーザの `AuthId` を問い合わせるための関数のうち、よく使うものとしては `maybeAuthId`、`requireAuthId`、`maybeAuth`、`requireAuth` などがあります。"require" が付く関数はユーザがログインしていなければログインページにリダイレクトします。また、関数名が `Id` で終わらない関数はテーブルIDとエンティティ値の両方を返します。

`AuthId` の全てのストレージはセッション上に作られるため、セッションのルールが適用されます。特に、データは暗号化された上でHMAC化したクライアントクッキーに保存され、アクティブでない期間が設定した値を超えると自動的にタイムアウトします。さらに、セッションにはサーバサイドの要素は何も含まれないため、ログアウトはセッションクッキーからデータを削除するだけです。そのため、仮にユーザが古いクッキー値を再利用したとしても、そのセッションはまだ有効なはずです。

<div class=yesod-book-notice>
もし望むのであれば、デフォルトのクライアントサイドセッションをサーバサイドセッションに置き換えることで、強制ログアウト機能を実装できます。また、中間者 (MITM) 攻撃からユーザのセッションを保護したいのであれば、SSL を有効にし、セッションの章で紹介した `sslOnlySessions` や `sslOnlyMiddleware` を使ってセッションを堅牢にすべきです。
</div>

認証が yesod-auth パッケージによって処理される一方で、認可は `Yesod` 型クラスのいくつかのメソッドによって処理されます。各リクエストに対してこれらのメソッドは、アクセスが許可/拒否されているか、ユーザを認証する必要があるかどうかを確認するために実行されます。デフォルトでは、これらのメソッドはどのリクエストも許可しています。型クラスを利用せずに、個々のハンドラ関数に `requireAuth` の呼び出しを追加することで、認可をよりアドホックに実装することもできます。しかし、このやり方は宣言的な認可システムの多くの利点を損ねることになります。

## 自分を認証してみよう

認証の例に取り組んでみましょう。Google OAuth 認証を動かすためには、次のステップに従う必要があります。

1. [Google Developers Help](https://developers.google.com/identity/protocols/OAuth2) を読んで OAuth 2.0 認証情報の取得方法について確認しましょう。認証情報はクライアントIDと、Google とあなたのアプリケーションだけが共有するクライアントシークレットです。
2. `Authorized redirect URIs` を `http://localhost:3000/auth/page/googleemail2/complete` に設定します。
3. `Google+ API` と `Contacts API` を有効にしましょう。
4. 取得した `clientId` と `secretID` の値を、以下のコードの `clientId` と `clientSecret` にセットしましょう。

``` haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.Default                (def)
import           Data.Text                   (Text)
import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.BrowserId
import           Yesod.Auth.GoogleEmail2


-- Replace with Google client ID.
clientId :: Text
clientId = ""

-- Replace with Google secret ID.
clientSecret :: Text
clientSecret = ""

data App = App
    { httpManager :: Manager
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    -- Note: In order to log in with BrowserID, you must correctly
    -- set your hostname here.
    approot = ApprootStatic "http://localhost:3000"

instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins _ =
        [ authBrowserId def
        , authGoogleEmail clientId clientSecret
        ]

    -- The default maybeAuthId assumes a Persistent database. We're going for a
    -- simpler AuthId, so we'll just do a direct lookup in the session.
    maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

main :: IO ()
main = do
    man <- newManager
    warp 3000 $ App man
```

ルートの宣言から確認していきましょう。まずは、標準的な `HomeR` ルートを宣言します。次に、認証サブサイトを設定します。サブサイト宣言には、サブサイトへのパス、ルート名、サブサイト名、サブサイト値を取得するための関数という、4つのパラメータが必要でした。つまり、以下のようになります。

```haskell
/auth AuthR Auth getAuth
```

ここで `getAuth :: MyAuthSite → Auth` を用意する必要がありますが、自分でこの関数を書かなくても yesod-auth が自動的に用意してくれます。他のサブサイト (静的ファイルなど) では、サブサイト値に構築設定を記述するため、取得用の関数を用意する必要があります。auth サブサイトでは、これらの設定は別の `YesodAuth` 型クラスで行います。

<div class="yesod-book-notice">
なぜ、サブサイト値を使わないのでしょうか？auth サブサイトに対して与えたい設定はいくつもありますが、レコード型から設定するのは不便でしょう。また、`AuthId` 関連型を持ちたいので、型クラスの方が自然です。では、なぜ全てのサブサイトで型クラスを用いないのでしょうか？その答えは、型クラスを利用するということは各サイトにつきインスタンスを1つしか定義できないことになります。つまり、異なる静的ファイルを異なるルートから取得できなくなってしまいます。また、サブサイト値はアプリケーションの初期化時にデータを読み込みたいという時に、よりうまく機能します。
</div>

では、この `YesodAuth` インスタンスでは実際に何が起こっているのでしょうか？この型クラスには6つの必須宣言があります。

- `AuthId` は関連型です。これは、ユーザがログインしているかどうか (`maybeAuthId` または `requireAuthId` を通して) を尋ねた時に `yesod-auth` が与えてくれる値です。今回の場合は単純に `Text` を使いました。後の例では、未加工の識別子 (今回はメールアドレス) を保存します。
- `getAuthId` は実際の `AuthId` を `Creds` (credentials) 型から取得します。この型は3つの情報を持ちます。使用されている認証バックエンド (今回の場合は googleemial)、実際の識別子、そして、任意の追加情報のための連想リストです。バックエンドによっては異なる追加情報が必要になります。詳細についてはドキュメントを参照してください。
- `loginDest` はログインが成功した後のリダイレクト先を設定します。
- 同様に、`logoutDest` はログアウト後のリダイレクト先を設定します。
- `authPlugins` は使用する個々の認証バックエンドリストです。今回の例では、Google OAuth による Google アカウントを使ったユーザ認証を実装しました。

これら5つのメソッドに加えて、ログインページの見た目のような認証システムの別の側面を制御するためのメソッドも同様に用意されています。詳細については [API ドキュメント](https://www.stackage.org/package/yesod-auth) を参照してください。

今回の `HomeR` ハンドラではユーザがログイン状態によって、ログイン/ログアウトページへの簡単なリンクが出現します。ここでは、サブサイトへのリンクの作り方に注目してください。まずは、サブサイトのルート名 (`AuthR`) 、次に、サブサイト内のルートを与えるだけです (`LoginR` と `LogoutR`)。

## Email

多くの使用例において, 第3者によるemailの認証で十分である. 時々, ユーザがサイト上でパスワードを作れるようにしたいと思うだろう. scaffoldedサイトは以下の理由で, この設定を含まない:

- 安全にパスワードを受理するためには, SSL上で実行する必要がある. 多くのユーザはSSL上でサイトを実行していない. 

- emailバックエンドは適切にパスワードにソルトとハッシュ行うが, 欠陥のあるデータベースは依然として問題がある. また, Yesodユーザが安全なデプロイメントプラクティスに則っていることを想定していないのである.

- emailを送るためのワークシステムが必要である. 最近, 多くのウェブサーバはメールサーバによって用いられているような, あらゆるスパム防御処置を行う準備ができていないのである. 

<div class=yesod-book-notice>
下の例では, システム組込のsendmailコマンドを用いる. もし自分のemailサーバを用いる面倒さを避けたいならば, Amazon SESを用いることができる. [mime-mail-ses](https://www.stackage.org/package/mime-mail-ses)と呼ばれるパッケージが存在し, それは下のemailを送るコードで用いられているように, ちょっとした代わりの方法を与える. これはたいてい私が推奨する方法であり, FP Haskell CenterやHaskellers.comのような私の大部分のサイトでも用いられている.
<div>

しかし, これらの要望を叶え, 独自のサイトに特化したパスワードログインを持ちたいと想定した時, Yesodは組込のバックエンドを提供する. それは, データベースにパスワードを安全に保管する必要があり, 多くの異なるemailをユーザに送信する必要があるため(アカウントの検証や, パスワード修復など), 設定にかなり多くのコードを要する. 

email認証を与えるサイトを作り, パスワードをPersistent SQLiteデータベースに保存してみよう. 

<div class=yesod-book-notice>
emailサーバを持っていなくても, デバック目的のために, リンクがコンソールに表示される. 
<div>


``` haskell
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad            (join)
import           Control.Monad.Logger (runNoLoggingT)
import           Data.Maybe               (isJust)
import           Data.Text                (Text, unpack)
import qualified Data.Text.Lazy.Encoding
import           Data.Typeable            (Typeable)
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Network.Mail.Mime
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Hamlet              (shamlet)
import           Text.Shakespeare.Text    (stext)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Email

share [mkPersist sqlSettings { mpsGeneric = False }, mkMigrate "migrateAll"] [persistLowerCase|
User
    email Text
    password Text Maybe -- Password may not be set yet
    verkey Text Maybe -- Used for resetting passwords
    verified Bool
    UniqueUser email
    deriving Typeable
|]

data App = App SqlBackend

mkYesod "App" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    -- Emails will include links, so be sure to include an approot so that
    -- the links are valid!
    approot = ApprootStatic "http://localhost:3000"
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Set up Persistent
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB f = do
        App conn <- getYesod
        runSqlConn f conn

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [authEmail]

    -- Need to find the UserId for the given email address.
    getAuthId creds = runDB $ do
        x <- insertBy $ User (credsIdent creds) Nothing Nothing False
        return $ Just $
            case x of
                Left (Entity userid _) -> userid -- newly added user
                Right userid -> userid -- existing user

instance YesodAuthPersist App

-- Here's all of the email-specific code
instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    afterPasswordRoute _ = HomeR

    addUnverified email verkey =
        runDB $ insert $ User email Nothing (Just verkey) False

    sendVerifyEmail email _ verurl = do
        -- Print out to the console the verification email, for easier
        -- debugging.
        liftIO $ putStrLn $ "Copy/ Paste this URL in your browser:" ++ unpack verurl

        -- Send email.
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                [stext|
                    Please confirm your email address by clicking on the link below.

                    #{verurl}

                    Thank you
                |]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml
                [shamlet|
                    <p>Please confirm your email address by clicking on the link below.
                    <p>
                        <a href=#{verurl}>#{verurl}
                    <p>Thank you
                |]
            , partHeaders = []
            }
    getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
    setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]
    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just u -> do
                update uid [UserVerified =. True]
                return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail = email
                }
    getEmail = runDB . fmap (fmap userEmail) . get

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

main :: IO ()
main = runNoLoggingT $ withSqliteConn "email.db3" $ \conn -> liftIO $ do
    runSqlConn (runMigration migrateAll) conn
    warp 3000 $ App conn
```

## 認可

一旦ユーザを認証すれば, その認証情報を用いて, リクエストを認可できる. Yesodにおける認可は単純であり, 宣言的である: 多くの場合, `authRoute`と`isAuthorized`メソッドをYesod型クラスのインスタンスに追加すればよいだけである. 例を見てみよう.

``` haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.Default         (def)
import           Data.Text            (Text)
import           Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy -- just for testing, don't use in real life!!!

data App = App
    { httpManager :: Manager
    }

mkYesod "App" [parseRoutes|
/      HomeR  GET POST
/admin AdminR GET
/auth  AuthR  Auth getAuth
|]

instance Yesod App where
    authRoute _ = Just $ AuthR LoginR

    -- route name, then a boolean indicating if it's a write request
    isAuthorized HomeR True = isAdmin
    isAuthorized AdminR _ = isAdmin

    -- anyone can access other pages
    isAuthorized _ _ = return Authorized

isAdmin = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "You must be an admin"

instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins _ = [authDummy]

    maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Note: Log in as "admin" to be an administrator.
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            <p>
                <a href=@{AdminR}>Go to admin page
            <form method=post>
                Make a change (admins only)
                \ #
                <input type=submit>
        |]

postHomeR :: Handler ()
postHomeR = do
    setMessage "You made some change to the page"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = defaultLayout
    [whamlet|
        <p>I guess you're an admin!
        <p>
            <a href=@{HomeR}>Return to homepage
    |]

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    warp 3000 $ App manager
```

`authRoute`はログインページであるべきである, ほとんど常に, `AuthR`, `LoginR`である. `isAuthorized`は2つのパラメータを取る関数である: リクエストされたルートと, リクエストが"write"なリクエストであるかどうか, である. 実際に, `isWriteRequest`を用いることで, writeリクエストが何であるかを変更できるが, 創造的な方法では, RESTful原則に従う: `GET`, `HEAD`, `OPTIONS`, あるいは`TRACE`リクエストを除いて, 全てwriteリクエストである. 

`isAuthorized`の中身に関し便利な点は, 望む`Handler`コードを何でも実行できることである. これは以下を意味する:

- ファイルシステムにアクセスする(通常のIO)

- データベースから値を探す

- 欲しいセッションやリクエスト値を取ってくる

これらの技術を用い, 望むような洗練された認可システムを開発したり, 組織によって用いられてる既存システムに連動させることができる.

## 結論

この章においては, ユーザの認証を行う設定の基礎と, どのように組込の認可関数が, ユーザにとって簡易で, 宣言的な方法を与えるか, について説明した. これらは複雑な概念であるが, Yesodは独自のカスタム化された認可方法を作るための, 根幹を与えるはずである. 




















