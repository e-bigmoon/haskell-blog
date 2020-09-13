---
title: Wiki: markdown, chat subsite, event source
date: 2020/08/29
---

この例では, いくつかの異なる例をつなぎ合わせる. まずチャットサブサイトから始める. これによりチャットWidgetをあらゆるページに埋め込むことが可能になる. HTML 5イベントソースAPIを用いて, サーバからクライアントへのイベントの送信を扱う. 

## サブサイト: データ

サブサイトを定義するために, サブサイトのファウンデーション型を作る必要がある. これは, 通常のYesodアプリーケションに対して行うのと同じである. 今回の場合, 全てのイベントのチャンネルが, チャットの各参加者に送られるようにしたい. これは次のようになる:

``` haskell
-- @Chat/Data.hs
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Chat.Data where

import           Blaze.ByteString.Builder.Char.Utf8  (fromText)
import           Control.Concurrent.Chan
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import           Network.Wai.EventSource
import           Network.Wai.EventSource.EventStream
import           Yesod
import           Yesod.Core.Types (SubHandlerFor)

-- | Our subsite foundation. We keep a channel of events that all connections
-- will share.
data Chat = Chat (Chan ServerEvent)
```

同じモジュールにサブサイトのルートも定義する必要がある. 2つのコマンド必要とする: 1つは, 新しいメッセージを全てのユーザに送るためのもので, もう1つは, メッセージのストリームを受け取るためのものである. 

``` haskell
-- @Chat/Data.hs
mkYesodSubData "Chat" [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]
``` 

## サブサイト: ハンドラ

これでファウンデーションとルートを定義できた. サブサイトのディスパッチ機能を与えるために別々のモジュールが必要になる. このモジュールを`Chat`と呼び, ここからサブサイトがどのように機能するかを見る. 

サブサイトは常に, ユーザによって与えられるマスターサイトの上のレイヤとして存在する. 多くの場合, サブサイトはマスターサイトに存在するために, 特別な機能が必要である. 今回のチャットサブサイトにおいては, マスターサイトによりユーザ認証をしてもらいたい. サブサイトは現在のユーザがログインしているかどうかについて, 問い合わせを行い, ユーザ名を得られるようにする必要がある. 

この概念を表現するための方法は, 必要な機能を内包する型クラスを定義することである. `YesodChat`型クラスは次のようになる:

``` haskell
-- @Chat/Data.hs
class (Yesod master, RenderMessage master FormMessage)
        => YesodChat master where
    getUserName :: HandlerFor master Text
    isLoggedIn :: HandlerFor master Bool
```

チャットサブサイトを使うマスターサイトは`YesodChat`インスタンスを与える必要がある. (少し後に, この要求がどのように実行されるかについて見る.) いくつかの注目すべき面白いことがある:

- マスターサイトに対し, さらに制約をすることができる. 例えば, `Yesod`インスタンスを与えたり, フォームメッセージのレンダリングを許可したりできる. 前者によって, `defaultLayout`を使うことが可能になり, 後者により標準フォームWidgetを使うことが可能になる. 

- 以前この本では, `Handler`モナドをかなり多く使った. `Handler`は単に`HandlerFor`のアプリケーション特有の型注釈であることを思い出してもらいたい. このコードは多くの異なるアプリケーションにおいて機能することを意図しているため, トランスフォーマに対し, `HandlerFor`の完全形を用いる. 

`Handler`型注釈について, サブサイトにも同じようなものを持ちたい. 問題点としては: このモナドがどのようになるか?, ということである. サブサイトの場合, サブサイトデータ型とマスターサイト型を持つ`SubHandlerFor`を用いる. また, このために補助的な注釈を定義するが, それは, `YesodChat`インスタンスをマスターサイト型に要求し, 次のように表すことができる. 

``` haskell
-- @Chat/Data.hs
type ChatHandler a =
    forall master. YesodChat master =>
    SubHandlerFor Chat master a
```

これで部品が整ったので, サブサイトハンドラを書くことにしよう. 2つのルートがありました: 1つは, メッセージを送信するためのもので, もう1つはメッセージを受信するためのものである. まず, 送信から始めましょう. 以下のことが必要になる:

1. メッセージを送信するための人のユーザ名を取得する. 

2. 流入するパラメータからメッセージをパーズする. (クライアント側のAjaxコードを容易にするために, GETパラメータを用いる.)

3. `Chan`にメッセージを書く.

このコード全体で最も技巧的な部分は, いつ`lift`を用いるかについて知ることである. 実装を見て, それらの`lift`の用い方について議論しましょう:

``` haskell
-- @Chat/Data.hs
postSendR :: ChatHandler ()
postSendR = do
    from <- liftHandler getUserName
    body <- runInputGet $ ireq textField "message"
    Chat chan <- getSubYesod
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $
        fromText from <> fromText ": " <> fromText body
```

`getUserName`は早期に`YesodChat`型クラスで定義された関数である. その型注釈を見れば, それはマスターサイトのハンドラモナドにあることが分かるであろう. したがって, その呼び出しをサブサイトから`lift`する必要がある. 

`runInputGet`の呼び出しについては, もっと微妙である. 理論的には, これはサブサイトでもマスターサイトでも実行可能である. しかし, ここでは1つの特別な理由のため`lift`を用いる: それはメッセージの翻訳である. マスターサイトを用いることで, マスターサイトの定義する全ての`RenderMessage`インスタンスを利用することができる. これはまた, なぜ`YesodChat`型クラスに`RenderMessage`の制約を与えるかについての理由にもなっている. 

次の呼び出しである`getSubYesod`は`lift`されない. その理由は単純である: メッセージチャンネルにアクセスするために, サブサイトのファウンデーション型が欲しいためである. もし仮にその呼び出しを`lift`した場合, 代わりにマスターサイトのファウンデーション型を取得することになってしまい, それはこの場合に望むものではない. 

最後の行では新しいメッセージをチャンネルに入力する. これは`IO`アクションであるため, `liftIO`を用いる. `ServerEvent`は`wai-eventsource`パッケージの一部であり, この例においてサーバから送信されるメッセージを与えるための方法である. 

受信の側については同様に単純である:

``` haskell
-- @Chat/Data.hs
getReceiveR :: ChatHandler ()
getReceiveR = do
    Chat chan <- getSubYesod
    sendWaiApplication $ eventSourceAppChan chan
```

この関数における最後の行は, 根底にある`wai-eventsource`アプリケーションをYesodハンドラとして露呈する. ここでは, `sendWaiApplication`を用いることで, WAIアプリケーションをYesodハンドラに昇格している. `eventSourceAppChan`は内部においてchanを複製しているが, これは並行Haskellにおいて, ブロードキャストチャンネルを作るための標準的な方法である. 

今やハンドラ関数を定義できたので, ディスパッチを設定できる. 一般的なアプリケーションにおいては, ディスパッチは`mkYesod`を呼び出すことで扱われ, それは適切な`YesodDispatch`インスタンスを作る. サブサイトにおいては, もう少し複雑である. なぜならば, しばしばマスターサイトに制約を付したいためである. 今回使われる形式は以下のようになる:

``` haskell
-- @Chat.hs
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Chat where

import           Chat.Data
import           Yesod

instance YesodChat master => YesodSubDispatch Chat master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChat)
```

今回の`Chat`サブサイトは`YesodChat`クラスのインスタンスであるどんなマスターサイトの上にも構築可能である. そして, `mkYesodSubDispatch`Template Haskell関数を用いて, 全てのディスパッチロジックを生成できる. これは`mkYesod`を書くよりも少し難しいが, 必要な柔軟性を与え, 書こうとするどんなサブサイトにおいても大部分が同一である. 

## サブサイト: Widget

今や完全に機能するサブサイトを作ることができた. チャットライブラリとして求める最後の要素は, Widgetがページに埋め込まれ, チャット機能を提供することである. Widgetとしてこれを作ることで, あらゆるHTML, CSS, Javascriptを再利用可能なコンポーネントとして用いることができる.

今回のWidgetは引数を1つ取る必要がある: `Chat`サブサイトURLをマスターサイトURLに変換する関数である. この根拠としては, アプリケーション開発者はチャットサブサイトをURL構造のどこにでも配置することができ, このWidgetは正しいURLを指し示すJavascriptを生成できる必要があるためである. Widgetの部分から始めよう:

``` haskell
-- @Chat.hs
chatWidget :: YesodChat master
           => (Route Chat -> Route master)
           -> WidgetFor master ()
chatWidget toMaster = do
```

次に, Widgetによって用いられる識別子を生成する. 名称の衝突をさけるために, 手動で作る代わりにYesodに唯一の識別子を生成させることが, 常によい慣習である.

``` haskell
-- @Chat.hs
    chat <- newIdent   -- the containing div
    output <- newIdent -- the box containing the messages
    input <- newIdent  -- input field from the user
```

次に, ユーザがログインしているかを確認する必要がある. そこでは, `YesodChat`型クラスにおける`isLoggedIn`関数を用いる. ここでは`Widget`にいるが, その関数は`Handler`モナドにいるため, `handlerToWidget`を用いる必要がある:

``` haskell
-- @Chat.hs
    ili <- handlerToWidget isLoggedIn  -- check if we're already logged in
```

もしユーザがログインしていれば, チャットボックスを表示し, それをCSSスタイリングし, Javascriptを用いてそれを相互作用できるようにしたい. これは大部分がクライアント側のコードであり, Widgetにラップされている:

``` haskell
-- @Chat.hs
    if ili
        then do
            -- Logged in: show the widget
            [whamlet|
                <div ##{chat}>
                    <h2>Chat
                    <div ##{output}>
                    <input ##{input} type=text placeholder="Enter Message">
            |]
            -- Just some CSS
            toWidget [lucius|
                ##{chat} {
                    position: absolute;
                    top: 2em;
                    right: 2em;
                }
                ##{output} {
                    width: 200px;
                    height: 300px;
                    border: 1px solid #999;
                    overflow: auto;
                }
            |]
            -- And now that Javascript
            toWidgetBody [julius|
                // Set up the receiving end
                var output = document.getElementById(#{toJSON output});
                var src = new EventSource("@{toMaster ReceiveR}");
                src.onmessage = function(msg) {
                    // This function will be called for each new message.
                    var p = document.createElement("p");
                    p.appendChild(document.createTextNode(msg.data));
                    output.appendChild(p);

                    // And now scroll down within the output div so the most recent message
                    // is displayed.
                    output.scrollTop = output.scrollHeight;
                };

                // Set up the sending end: send a message via Ajax whenever the user hits
                // enter.
                var input = document.getElementById(#{toJSON input});
                input.onkeyup = function(event) {
                    var keycode = (event.keyCode ? event.keyCode : event.which);
                    if (keycode == '13') {
                        var xhr = new XMLHttpRequest();
                        var val = input.value;
                        input.value = "";
                        var params = "?message=" + encodeURI(val);
                        xhr.open("POST", "@{toMaster SendR}" + params);
                        xhr.send(null);
                    }
                }
            |]
```

そして最後に, もしユーザがログインしていなければ, チャットアプリを使うためにログインするように促す.

``` haskell
-- @Chat.hs
        else do
            -- User isn't logged in, give a not-logged-in message.
            master <- getYesod
            [whamlet|
                <p>
                    You must be #
                    $maybe ar <- authRoute master
                        <a href=@{ar}>logged in
                    $nothing
                        logged in
                    \ to chat.
            |]
```

## マスターサイト: データ

今やメインのアプリケーションを書くことを進められる. このアプリケーションはチャットサブサイトとwikiを含む. 最初に考慮すべきことは, どのようにwikiコンテンツを保管するかについてである. 一般的に, これをある種のパージステントデータベースに置きたいと思う. 簡単のため, 内部メモリによる表現を用いる. 各wikiページは, 名前のリストにより指し示され, 各ページのコンテンツは`Text`になる. すると, 完全なファウンデーションのデータ型は次にようになる.

``` haskell
-- @ChatMain.hs
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module ChatMain where

import           Chat
import           Chat.Data
import           Control.Concurrent.Chan (newChan)
import           Data.IORef
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text.Lazy          as TL
import           Text.Markdown
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy
import           System.SetEnv

data App = App
    { getChat     :: Chat
    , wikiContent :: IORef (Map [Text] Text)
    }
```

次にルートを設定する.

``` haskell
-- @ChatMain.hs
mkYesod "App" [parseRoutes|
/            HomeR GET      -- the homepage
/wiki/*Texts WikiR GET POST -- note the multipiece for the wiki hierarchy

/chat        ChatR Chat getChat    -- the chat subsite
/auth        AuthR Auth getAuth    -- the auth subsite
|]
```

## マスターサイト: インスタンス

デフォルトの`Yesod`インスタンスに対し, 2つの変更をする必要がある. まず, `authRoute`の実装を与え, サブサイトwidgetがログインページへの適切なリンクを表示できるようにしたい. 次に, `defaultLayout`をオーバーライドしたい. ログイン/ログアウトリンクを与える以外に, この関数は全ページのチャットwidgetに追加される. 

``` haskell
-- @ChatMain.hs
instance Yesod App where
    authRoute _ = Just $ AuthR LoginR -- get a working login link

    -- Our custom defaultLayout will add the chat widget to every page.
    -- We'll also add login and logout links to the top.
    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            widget
            chatWidget ChatR
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
                            <div .message>#{msg}
                        <nav>
                            <a href=@{AuthR LoginR}>Login
                            \ | #
                            <a href=@{AuthR LogoutR}>Logout
                        ^{pageBody pc}
            |]
``` 

チャットサブサイトを用いているため, `YesodChat`のインスタンスを与える必要がある. 

``` haskell
-- @ChatMain.hs
instance YesodChat App where
    getUserName = do
        muid <- maybeAuthId
        case muid of
            Nothing -> do
                setMessage "Not logged in"
                redirect $ AuthR LoginR
            Just uid -> return uid
    isLoggedIn = do
        ma <- maybeAuthId
        return $ maybe False (const True) ma
```

`YesodAuth`と`RenderMessage`インスタンスは, ホームページのハンドラ同様かなり単純である:

``` haskell
-- @ChatMain.hs
-- Fairly standard YesodAuth instance. We'll use the dummy plugin so that you
-- can create any name you want, and store the login name as the AuthId.
instance YesodAuth App where
    type AuthId App = Text
    authPlugins _ = [authDummy]
    loginDest _ = HomeR
    logoutDest _ = HomeR
    getAuthId = return . Just . credsIdent
    maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Nothing special here, just giving a link to the root of the wiki.
getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>Welcome to the Wiki!
        <p>
            <a href=@{wikiRoot}>Wiki root
    |]
  where
    wikiRoot = WikiR []
```

## マスターサイト: wikiハンドラ

さあ, wikiハンドラを書きましょう: GETはページを表示するためのもので, POSTはページを更新するためのものである. また, `wikiForm`関数を作成し, 両方のハンドラにおいて用いる. 

``` haskell
-- @ChatMain.hs
-- A form for getting wiki content
wikiForm :: Maybe Textarea -> Html -> MForm Handler (FormResult Textarea, Widget)
wikiForm mtext = renderDivs $ areq textareaField "Page body" mtext

-- Show a wiki page and an edit form
getWikiR :: [Text] -> Handler Html
getWikiR page = do
    -- Get the reference to the contents map
    icontent <- fmap wikiContent getYesod

    -- And read the map from inside the reference
    content <- liftIO $ readIORef icontent

    -- Lookup the contents of the current page, if available
    let mtext = Map.lookup page content

    -- Generate a form with the current contents as the default value.
    -- Note that we use the Textarea wrapper to get a <textarea>.
    (form, _) <- generateFormPost $ wikiForm $ fmap Textarea mtext
    defaultLayout $ do
        case mtext of
            -- We're treating the input as markdown. The markdown package
            -- automatically handles XSS protection for us.
            Just text -> toWidget $ markdown def $ TL.fromStrict text
            Nothing -> [whamlet|<p>Page does not yet exist|]
        [whamlet|
            <h2>Edit page
            <form method=post>
                ^{form}
                <div>
                    <input type=submit>
        |]

-- Get a submitted wiki page and updated the contents.
postWikiR :: [Text] -> Handler Html
postWikiR page = do
    icontent <- fmap wikiContent getYesod
    content <- liftIO $ readIORef icontent
    let mtext = Map.lookup page content
    ((res, form), _) <- runFormPost $ wikiForm $ fmap Textarea mtext
    case res of
        FormSuccess (Textarea t) -> do
            liftIO $ atomicModifyIORef icontent $
                \m -> (Map.insert page t m, ())
            setMessage "Page updated"
            redirect $ WikiR page
        _ -> defaultLayout
                [whamlet|
                    <form method=post>
                        ^{form}
                        <div>
                            <input type=submit>
                |]
```

## マスターサイト: 実行

やっと, アプリケーションを実行する準備ができた. この本における以前の多くの例と異なり, `main`関数において実際の初期化を行う必要がある. `Chat`サブサイトは, 空の`Chan`を作られる必要がある. また,  wikiコンテンツを格納するためのmutable変数を作る必要がある. 一度これらの値を作ってしまえば, `App`値を作成し, それを`warp`関数に渡すことができる. 

``` haskell
-- @ChatMain.hs
main :: IO ()
main = do
    -- Create our server event channel
    chan <- newChan

    -- Initially have a blank database of wiki pages
    icontent <- newIORef Map.empty

    -- Set web server's listening port required by warpEnv function
    -- This env var is set up automatically if 'yesod devel' is used
    setEnv "PORT" "3000"

    -- Run our app
    warpEnv App
        { getChat = Chat chan
        , wikiContent = icontent
        }
```

## 結論

この例では非自明なサブサイトの作り方について説明した. 大切な点は, マスターサイトへの制約を表す型クラスの使い方, `main`関数におけるデータの初期化方法と, `lift`ingにより, どのようにしてサブサイトとマスターサイトの両方の文脈で操作することが可能になるか, という点である. 

もしサブサイトのスキルを試す方法を探しているのなら, この例を変更して, Wikiコードも自身のサブサイト上に所属しているようにすることを推奨する.