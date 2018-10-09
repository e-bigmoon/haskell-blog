# Authentication and Authorization

認証と承認は2つの非常に関連した, しかし異なる概念である. 前者はユーザを特定することに関与し, 後者はユーザに許可されたことを決定する. 残念なことに, 両方の用語はしばしば"auth"と省略されるため, その概念はしばしば融合されてしまう.

YesodはOpenID, BrowserIDやOAUTHのような多くの第3者認証システムに対し, 組込式のサポートを提供している. これらは, アプリケーションがユーザの認証情報を有効にするための外部システムを信頼するシステムである. さらに, より一般的に用いられるusername/passwordやemail/passwordシステムに対するサポートもある. 最初のルートはユーザにとっての簡便性を保証し(新しいパスワードを覚えずに済む), 後ろのルートは実装者にとっての簡便性を保証する(セキュリティ構造全体を扱う必要がなくなる). 一方, 後ろのルートの方はより開発者に制御されている. 

承認側では, RESTと型安全URLを利用し, 簡便で宣言的なシステムを作る. さらに, すべての承認コードはHaskellで書かれているため, 言語を完全に柔軟かつ自由に使うことができる. 

この章では, Yesodにおいてどのように"auth"解決策を構成するかについて示し, 異なる認証システム選択におけるトレードオフについて論ずる.

# 概略

yesod-authパッケージはいくつもの異なる認証プラグインのための, 統一的インターフェースを与える. これらのバックエンドに対し, 唯一の実際に必要なものはユーザをある一意的な文字列で特定することである. 例えばOpenIDにおいては, それはemailアドレスに相当する. HasDB(ハッシュ化されたパスワードのデータベースを用いる)においては, ユーザ名に相当する. 

各認証プラグインは, ログインのための独自システムを与える. そして, それは外部サイトとトークンや, email/password形式であったりする. ログインが成功すると, プラグインはユーザのセッションに値を設定し, 彼/彼女の`AuthId`を指定する. この`AuthID`はたいていは, ユーザを追跡するために用いられるテーブルからのPersistent IDである. 

ユーザの`AuthID`をクエリするための関数はいくつか存在し, 最も一般的なものは, `maybeAuthID`
, `requireAuthID`, `maybeAuth`そして, `requireAuth`である. "require"の方は, ユーザがログインしていなければログインページにリダイレクトし, 2つめの関数のセット(`Id`で終わっていない関数)は, テーブルIDとエンティティ値の両方を与える. 

`AuthID`の全てのストレージは, セッションの頂点に構築されているため, そこにおける全てのルールが適用される. 特に, データは暗号化され, HMAC化したクライアントクッキーに保存され, ある設定可能な時間の間, 使用されなければ, 自動的にタイムアウトする. さらに, セッションにはサーバサイドの要素が存在しないため, ログアウトによりセッションクッキーからデータが削除される;もし, ユーザが古いクッキー値を用いていれば, セッションはまだ有効である.

<div class=yesod-book-notice>
もし望むのなら, デフォルトのクライアントサイドセッションをサーバサイドセッションに置き換え, 強制ログアウト機能を与えることができる. また, もしセッションを中間者(MITM)攻撃からセッションを保護したいのなら, サイトをSSl上で作動させ, セッションの章で紹介した`sslOnlySessions`や`sslOnlyMiddleware`を用い, セッションを堅牢にすべきである. 
<div>

裏面, 承認は`Yesod`型クラスにおけるいくつかの関数により処理される. 各リクエストに対し, これらのメソッドは, アクセスが許可されているか, 拒否されているか, あるいは, もしユーザが承認される必要があるかどうかについてを決定するために実行される. デフォルトでは, これらのメソッドは, どのリクエストも許可している. 代わりに, 承認をよりアドホックな方法で実装し, `requireAuth`にようなものを各ハンドラに追加したりできる. しかし, これは宣言的認証システムの利点を損ねることになる. 

## 私を認証しなさい

認証の例に取り組んでみよう. Google oAuth認証が機能するためには, 次のステップに従う必要がある:

1. [Google Developers Help](https://developers.google.com/identity/protocols/OAuth2)で, どのようにして, Googleとアプリケーションに分かるクライアントIDやクライアント秘密情報のようなOAuth 2.0認証情報を得るかについて, 読みなさい.

2. `Authorized redirect URIs`を`http://localhost:3000/auth/page/googleemail2/complete`に設定しなさい.

3. `Google+ API`と`Contacts API`を有効にしなさい.

4. `clientId`と`secretID`を得たら, 下のコードで交換してみなさい.

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

ルートの宣言から始める. まず, 標準的な`HomeR`ルートを宣言し, 認証サブサイトを設定する. サブサイトは, 4つのパラメータが必要であることを思い出しなさい: サブサイトへのパス, ルート名, サブサイト名, そして, サブサイト値を得るための関数. コードに換言すれば, 次のようになる.

``` haskell
/auth AuthR Auth getAuth
```

`getAuth :: MyAuthSite → Auth`を持つ必要がある. 自身でその関数を書いてはいないが, yesod-authが自動的に与えてくれる. 他のサブサイト(static fileのような)では, サブサイト値における構築設定をする必要があり, それゆえget関数を明記する必要がある. authサブサイトでは, これらの設定は別の型クラスである`YesodAuth`で明記する.

<div class=yesod-book-notice>
なぜサブサイト値を使わないのか? authサブサイトに対して与えたい設定はいくつもあるが, レコード型からそのようにすることは不便であろう. また, `AuthID`関連型を持ちたいため, 型クラスがより自然である. そして, なぜ全てのサブサイトで型クラスを用いないのか? それは次の短所より来る: その場合, 各サイトにつき単一のインスタンスのみしか持てず, 異なるstaticファイルを異なるルートから得ることができなくなる. また, サブサイト値はアプリケーション初期化時にデータをロードする際に, よりうまく機能する. 
<div>

すると, この`YesodAuth`インスタンスでは実際に何が起こっているのか? そこには, 6つの必要な宣言が存在する:

- `AuthId`は関連型である. これは, ユーザがログインしているか(`maybeAuthId`または, `requireAuthId`を通して)尋ねる際`yesod-auth`が与えてくれる値である. 今回の場合, 単純に`text`を用い, すぐに分かるように, 生識別子-今回はemailアドレス, を保存している. 

- `getAuthId`は実際の`AuthId`を`Creds`(credentials)型から取得する. この型は, 3つの情報を持っている: 使用されている認証バックエンド(今回の場合, ブラウザid, または, googleemial), 実際の識別子, そして, 任意の追加情報についての関連するリスト. 各バックエンドは, 異なる追加情報を与える; 詳細については文書を参照しなさい.

- `loginDest`は, ログイン後リダイレクトするルートを与える.

- 同様に, `logoutDest`は, ログアウト後リダイレクトするルートを与える.

- `authPlugins`は使用する個々の認証バックエンドリストである. 今回の例では, BrowserIDを用いており, MozillaのBrowserIDシステムと, Google oAuthによってログインし, Googleアカウントを用いてユーザを認証する. BrowserIDバックエンドのちょっとして利点は次のようになる:

  - セットアップ認証情報が必要となるFacebookやOAuthとは対照的に, セットアップが不要である.

  - 識別子としてemailアドレスを用い, それはURLを用いるOpenIDとは対照的に, 人々にとって快適である.

- `authHttpManager`はfoundationデータ型から, HTTP通信マネージャを取得する. これによりHTTP通信(言い換えれば, 大部分の第3者ログインシステム)を用いる認証バックエンドが通信を共有し, TCP接続をリクエスト毎に再開始するコストを避けることができる. 

これら6つのメソッドに加え, ログインページがどのように見えるか, のような認証システムの別の側面を制御するのを可能にする, 他のメソッドが存在する. 詳細については, [API文書](https://www.stackage.org/package/yesod-auth)を参照しなさい.

今回の`HomeR`ハンドラでは, ユーザがログインしているかそうでないかにより, ログイン, ログアウトページへの簡単なリンクがある. どのようにしてこれらのサブサイトリンクを作ったかについて, 注意しなさい: まず, サブサイトのルート名(`AuthR`)を与え, サブサイト内のルートを与えた(`LoginR`と`LogoutR`).

下の写真は, ログインプロセスがユーザ視点からどのよに見えるかを示している.

### 最初にロードされるページ

![Initial page load](https://www.yesodweb.com/book/image/initial-screen)

### ブラウザIDログイン画面

![BrowserID login screen](https://www.yesodweb.com/book/image/login-with-browserid)

### ログインした後のホームページ

![Homepage after logging in](https://www.yesodweb.com/book/image/after-login)

## Email

多くの使用例において, 第3者によるemailの認証で十分である. 時々, ユーザがサイト上でパスワードを作れるようにしたいと思うだろう. scaffoldedサイトは以下の理由で, この設定を含まない:

- 安全にパスワードを受容するためには, SSL上で実行する必要がある. 多くのユーザあSSL上でサイトを実行していない. 

- emailバックエンドは適切にパスワードにソルトとハッシュ行うが, 欠陥のあるデータベースは依然として問題がある. また, Yesodユーザが安全なデプロイメント慣習に則っていることを想定していないのである.

- emailを送るためのワークシステムが必要である. 最近, 多くのウェブサーバはメールサーバによって用いられているような, あらゆるスパム防御処置を行う準備ができていないのである. 

<div class=yesod-book-notice>
下の例では, システム組込のsendmail処理を用いる. もし自分のemailサーバを用いる面倒さを避けたいならば, Amazon SESを用いることができる. [mime-mail-ses](https://www.stackage.org/package/mime-mail-ses)と呼ばれるパッケージが存在し, それは下のemailを送るコードで用いられているように, ちょっとした代わりの方法を与える. これはたいてい私が推奨する方法であり, FP Haskell CenterやHaskellers.comのような私の大部分のサイトでも用いられている.
<div>

しかし, これらの要望を叶え, 独自のサイトに特化したログインオアスワードを持ちたいと想定した時, Yesodは組込のバックエンドを提供する. それは, データベースにパスワードを安全に保管する必要があり, 多くの異なるemailをユーザに送信する必要があるため(アカウントの検証や, パスワード修復など), 設定にかなり多くのコードを要する. 

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

## 承認

一旦ユーザを認証すれば, その認証情報を用いて, リクエストを承認できる. Yesodにおける承認は単純であり, 宣言的である: 多くの場合, `authRoute`と`isAuthorized`メソッドをYesod型クラスのインスタンスに追加しればよいだけである. 例を見てみよう.

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

`authRoute`はログインページであるべきである. ほとんど常に, `AuthR`, `LoginR`, `isAuthorized`は2つのパラメータを取る: リクエストされたルートと, リクエストがホワイトなリクエストであるかどうか, である. 実際に, `isWriteRequest`を用いることで, writeリクエストが何であるかを変更できるが, 創造的な方法では, RESTful原則に従う: `GET`, `HEAD`, `OPTIONS`, あるいは`TRACE`リクエストはwriteリクエストである. 

`isAuthorized`の中身に関し便利な点は, 望む`Handler`コードを何でも実行できることである. これは以下を意味する:

- ファイルシステムにアクセスする(通常のIO)

- データベースから値を探す

- 欲しいセッションやリクエスト値を取ってくる

これらの技術を用い, 望むような洗練された承認システムを開発したり, 組織によって用いられてる既存システムに連動させることができる.

## 結論

この章においては, ユーザの認証を行う設定の基礎と, どのように組込の承認関数が, ユーザにとって簡易で, 宣言的な方法を与えるか, について説明した. これらは複雑な概念であるが, Yesodは独自のカスタム化された承認方法を作るための, 根幹を与えるはずである. 




















