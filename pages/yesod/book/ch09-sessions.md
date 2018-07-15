---
title: Sessions
date: 2018/03/18
---

## Sessions

HTTP はステートレスなプロトコルです。
これを欠点とみなす人もいますが、RESTful ウェブ開発の支持者はこれをプラスに考えます。
状態が無くなれば、スケーラビリティとキャッシングのような恩恵が自動的に得られます。
一般的には Haskell が純粋であることを利用して多くの並列化が可能になります。

RESTful アプリケーションでは、できる限りクライアントとの対話の状態を保存しない方が良いです。
しかし、時にはそうすることができないこともあるでしょう。
ショッピングカートのような機能は古くからある例ですが、適切なログイン処理のようなありふれた相互作用は、セッションを正しく利用することでかなり改善されます。

この章では、どのように Yesod がセッションデータを格納し、どのようにしてこのデータにアクセスするのかを説明し、セッションを最大限に活用するための特別な関数について見ていきます。

## Clientsession

Yesod から派生したパッケージの中でもクライアントセッションはかなり初期のものです。
このパッケージはクライアントサイドのクッキーにデータを保存するために、暗号化と署名を利用します。
暗号化により盗聴からユーザを守り、署名によってセッションの改ざんを防ぎます。

効率性の立場からみれば、データをクッキーに保存することは悪い発想のように思うかもしれません。
結局のところ、これはあらゆるリクエスト毎にデータが送られる必要があることを意味するからです。
しかし、実際にはクライアントセッションはパフォーマンスの面で非常に大きな恩恵を与えてくれます。

- リクエストをサービスするためにサーバサイドのデータベース検索は一切行われません
- 簡単に水平拡張可能です。それぞれのリクエストがレスポンスを送るために必要なあらゆる情報を含んでいるからです
- 必要以上のバンド幅のオーバヘッドを防ぐために、プロダクションサイトは静的コンテンツを異なるドメイン名から与えることができます。なので、リクエスト毎にセッションクッキーの伝達を行うことを省略できます。

セッションに何メガバイトもの情報を保存することは良くない利用方法ですが、多くのセッション実装では、この慣習に反することが推奨されています。
もし、本当にユーザのために巨大なストレージが必要な場合は、セッションに検索用のキーを保存して、実際のデータはデータベースに格納することが望ましいです。

クライアントセッションにおける全ての相互作用は Yesod によって内部的に処理されますが、動作を少しだけ調整する必要のある箇所がいくつかあります。

## Controlling sessions

デフォルトでは Yesod アプリケーションは自身のセッションストレージのためにクライアントセッションストレージを利用します。
暗号化キーはクライアントの `client-session-key.aes` から取得し、2時間のタイムアウトをセッションに付加している. (タイムアウトはクライアントが最後にサイトへリクエストを送った時間から測定されます。セッションが最初に作られてからではない点に注意してください。)
しかし、これらの挙動は Yesod 型クラスの `makeSessionBackend` メソッドを上書きすることで変更できます。

このメソッドを上書きする最もわかりやすい例は、単にセッション処理を停止することです。
そのためには `Nothing` を返すようにします。
もし、アプリケーションに全くセッションの必要性がなければ、それらを停止することでほんの少しパフォーマンスの向上が見込めます。
しかし、セッションの停止すると Cross-Site Request Forgery 保護のような機能も無効になるため、注意してください。

```haskell
instance Yesod App where
    makeSessionBackend _ = return Nothing
```

他に、クライアントセッションを使い続けますが、ファイルパスやタイムアウト値を変更することもできます。
そのためには `defaultClientSessionBackend` 補助関数を利用します。

```haskell
instance Yesod App where
    makeSessionBackend _ =
        fmap Just $ defaultClientSessionBackend minutes filepath
      where minutes = 24 * 60 -- 1 day
            filepath = "mykey.aes"
```

他にも、クライアントセッションをより詳細にコントロールする関数もいくつかありますが、それらを必要とすることは滅多に無いでしょう。
もし興味があれば `Yesod.Core` のドキュメントを見ると良いです。
サーバサイドセッションのような他の形態のセッションを実装することも可能です。
私の知る限りにおいてこの記事を書いている時点では、その他の実装は存在していません。

与えられたキーのファイルが存在しなければ、ランダムに生成されたキーのファイルが新規作成されます。
アプリケーションをプロダクション環境にデプロイする際は、予め生成されたキーを一緒に含めておきましょう。
でないと、新しいキーのファイルが生成され、今まで利用していた全てのセッションが無効化されてしまいます。
scaffolded サイトはこれを代わりに対処してくれます。

## Handening via SSL

HTTP 上のクライアントセッションはハイジャック脆弱性の可能性を含みます。
攻撃者は暗号化されていないトラフィックを読み取り、そこからセッションクッキーを盗み取り、そして同じクッキーでリクエストを行い、ユーザになりすまします。
この脆弱性はセッションが個人特定可能な情報や認証に関するものを含む場合、特に深刻です。

この驚異を打ち破る唯一の手段は全てのサイトを SSL 上で走らせ、ブラウザが HTTP でアクセスするのを防ぐことです。
SSL で運用するための解決策は Web サーバーレベルの話なので、 `warp-tls` のような Haskell を使う方法や、あるいは Amazon Elastic Load Balancer のような SSL が有効化されたロードバランサーを利用する方法などがある。

サイトがクッキーを安全でないコネクションを通して送信することを防ぐために、`makeSessionBackend` と `yesodMiddleware` を少し変更し、アプリケーションのセッションを拡張しましょう。
`sslOnlySessions` と `sslOnlyMiddleware` をそれぞれ `makeSessionBackend` と `yesodMiddleware` に追加します。

```haskell
instance Yesod App where
    makeSessionBackend _ = sslOnlySessions $
        fmap Just $ defaultClientSessionBackend 120 "mykey.aes"
    yesodMiddleware = (sslOnlyMiddleware 120) . defaultYesodMiddleware
```

`sslOnlySessions` によって、全てのセッションクッキーでセキュアビットがオンになるため、ブラウザはそれらを HTTP では送信しません。
`sslOnlyMiddleware` は全てのリスポンスに対して Strict-Transport-Security ヘッダを付加します。
それによって、特定の時間の間ブラウザがドメインとサブドメインに対し HTTP リクエストを行わないように指示できます。
`sslOnlyMiddleware` のタイムアウトが少なくともセッションタイムアウト以上の長さであるように設定します。
これらを一緒に使うことで、セッションクッキーが暗号化されずに送信されてしまわないことを保証します。

## Session Operations

多くのフレームワークのように Yesod のセッションはキーと値を格納します。
ベースとなるセッションAPIは4つの関数です。
`lookupSession` はキー (もし利用できれば) に対し値を取得します。
`getSession` は全てのキーと値のペアを取得します。
`setSession` はキーに対し値をセットします。
`deleteSession` はキーに対して値をクリアします。

```haskell
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import           Control.Applicative ((<$>), (<*>))
import qualified Web.ClientSession   as CS
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

getHomeR :: Handler Html
getHomeR = do
    sess <- getSession
    defaultLayout
        [whamlet|
            <form method=post>
                <input type=text name=key>
                <input type=text name=val>
                <input type=submit>
            <h1>#{show sess}
        |]

postHomeR :: Handler ()
postHomeR = do
    (key, mval) <- runInputPost $ (,) <$> ireq textField "key" <*> iopt textField "val"
    case mval of
        Nothing -> deleteSession key
        Just val -> setSession key val
    liftIO $ print (key, mval)
    redirect HomeR

instance Yesod App where
    -- Make the session timeout 1 minute so that it's easier to play with
    makeSessionBackend _ = do
        backend <- defaultClientSessionBackend 1 "keyfile.aes"
        return $ Just backend

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

main :: IO ()
main = warp 3000 App
```

## Messages

以前に少しだけ触れたセッションの利用目的の1つはメッセージです。
それは Web 開発における共通の問題を解決することになりました。
ユーザが `POST` リクエストを実行した時 Web アプリケーションは何らかの変更を行って、同時にユーザを新しいページにリダイレクトし、ユーザに成功メッセージを送りたいです。 (これは Post/Redirect/Get として知られています)
Yesod はこのワークフローを可能にするための関数ペアを提供しています。
`setMessage` はセッションに値を格納し、`getMessage` は最新のセッションから値を読み込み、再び表れないように古い値をクリアします。

`getMessage` は `defaultLayout` での利用が推奨されます。
なぜなら、全てのハンドラに `getMessage` を追加しなくても、利用可能なメッセージが即座にユーザに表示されるためです。

``` haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/            HomeR       GET
/set-message SetMessageR POST
|]

instance Yesod App where
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        mmsg <- getMessage
        withUrlRenderer
            [hamlet|
                $doctype 5
                <html>
                    <head>
                        <title>#{pageTitle pc}
                        ^{pageHead pc}
                    <body>
                        $maybe msg <- mmsg
                            <p>Your message was: #{msg}
                        ^{pageBody pc}
            |]

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <form method=post action=@{SetMessageR}>
            My message is: #
            <input type=text name=message>
            <button>Go
    |]

postSetMessageR :: Handler ()
postSetMessageR = do
    msg <- runInputPost $ ireq textField "message"
    setMessage $ toHtml msg
    redirect HomeR

main :: IO ()
main = warp 3000 App
```

## Ultimate Destination

ホラー映画と混同しないで下さい。
最終目的地はもともと Yesod の認証フレームワークのために開発された技術ですが、それはもっと広く利用できます。
ユーザが認証の必要なページリクエストを送ったとしましょう。
ユーザがまだログインしていなければ、ユーザをログインページに送る必要があります。
上手く設計された Web アプリケーションは、彼らをリクエストした最初のページに戻します。
それが、私たちが最終目的地と呼ぶものです。

`redirectUltDest` はユーザをそれぞれのセッションでセットされた最終目的地に送り、その値をセッションからクリアします。
目的地がセットされていない場合のため、デフォルトの目的地を引数に取ります。
セッションの設定のためには3つの選択肢があります。

- `setUltDest` は目的地を与えられた URL にセットします。URLはテキスト形式のURLでも型安全URLでも良いです
- `setUltDestCurrent` は目的地を現在のリクエストされたURLにセットします
- `setUltDestReferer` は `Referer` ヘッダ (ユーザを現在のページに誘導したページ) に基づいて目的地をセットします

さらに `clearUltDest` 関数があり、これは、もしセッションに最終目的地の値が存在すればそれを捨てます。

小さなサンプルアプリケーションを見てみましょう。
これは、ユーザがセッションに彼/彼女の名前をセットし、別のルートでユーザに彼/彼女の名前を尋ねる例です。
もし、ユーザの名前がまだセットされていなければ、ユーザは名前のセットページにリダイレクトされます。
そして、現在のページに戻ってくるために最終目的地がセットされます。

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/         HomeR     GET
/setname  SetNameR  GET POST
/sayhello SayHelloR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            <a href=@{SetNameR}>Set your name
        <p>
            <a href=@{SayHelloR}>Say hello
    |]

-- Display the set name form
getSetNameR :: Handler Html
getSetNameR = defaultLayout
    [whamlet|
        <form method=post>
            My name is #
            <input type=text name=name>
            . #
            <input type=submit value="Set name">
    |]

-- Retreive the submitted name from the user
postSetNameR :: Handler ()
postSetNameR = do
    -- Get the submitted name and set it in the session
    name <- runInputPost $ ireq textField "name"
    setSession "name" name

    -- After we get a name, redirect to the ultimate destination.
    -- If no destination is set, default to the homepage
    redirectUltDest HomeR

getSayHelloR :: Handler Html
getSayHelloR = do
    -- Lookup the name value set in the session
    mname <- lookupSession "name"
    case mname of
        Nothing -> do
            -- No name in the session, set the current page as
            -- the ultimate destination and redirect to the
            -- SetName page
            setUltDestCurrent
            setMessage "Please tell me your name"
            redirect SetNameR
        Just name -> defaultLayout [whamlet|<p>Welcome #{name}|]

main :: IO ()
main = warp 3000 App
```

## Summary

セッションは HTTP の持つステートレス性を回避するための代表的な手法です。
これは、我々が望むあらゆる動作を行うための逃げ道として考えるべきではありません。
Web アプリケーションにおけるステートレス性は美徳であり、それは可能な限り崇拝されるべきですが、状態を保存することが重要となる特別なケースもあります。

Yesod におけるセッション API はかなり単純です。
それはキーと値の格納を行い、よく利用される共通の関数をいくつか提供しています。
正しく使えば少しの代償で済むため、セッションが Web 開発において悪く目立つことは無いでしょう。
