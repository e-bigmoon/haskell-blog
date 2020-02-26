---
title: SQL Joins
date: 2020/01/11
---

# Yesod for Haskellers

この本の大部分は, 表面化において何が起こっているかの詳細についてあまり深掘りせず, どのように一般的なタスクを成し遂げるかについての実用的な情報を与えるために作られている. この本はHaskellについての知識を想定するが, 多くのHaskellライブラリの紹介のような典型的な形式には従っていない. 多くの熟練したHaskellerは, このように実装詳細を隠されることで不快になるかもしれない. この章の目的は, そのような不安に対処することである.

この章においては, 簡単な最小限のウェブアプリケーションから始め, より複雑な例を構築し, その途上でコンポーネントや型について説明を加える.

## Hello Warp

考えうる中で最も単純な最小限のアプリケーションから始めましょう. 

``` haskell

{-# LANGUAGE OverloadedStrings #-}
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, responseLBS)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3000 app

app :: Application
app _req sendResponse = sendResponse $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello Warp!"

```

少し待ちなさい, ここにはYesodは全くない! 心配しないで下さい, すぐにそこにたどり着くだろう. 我々は, ゼロから取り組んでおり, Yesodにおける第１段階はWAI, つまりウェブアプリケーションインターフェイスである. WAIはウェブサーバやテストフレームワークのようなウェブハンドラと, ウェブアプリケーションの間に位置する. 今回の場合, ハンドラは高パフォーマンスウェブサーバであるWarpであり, アプリケーションは`app`関数ある. 

この謎めいた`Application`の型は何であろうか? これは型シノニムであり, 次のように定義される:

``` haskell
type Application = Request
                -> (Response -> IO ResponseReceived)
                -> IO ResponseReceived
```

`Request`値はリクエストされたパスや, クエリストリング, リクエストヘッダ, リクエストボディ, クライアントのIPアドレスのような情報を含む. 2つ目の引数は"レスポンスを送る"関数である. 単にアプリケーションに`IO Response`を返させる代わりに, `bracket`関数の機能と同様に, WAIは完全な例外安全を許すために, 継続渡しスタイルを用いる.   
これは単純なディスパッチを行うために用いられる:


``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, pathInfo, responseLBS)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3000 app

app :: Application
app req sendResponse =
    case pathInfo req of
        ["foo", "bar"] -> sendResponse $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            "You requested /foo/bar"
        _ -> sendResponse $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            "You requested something else"
```

WAIはパスを個々の断片(文字列の手前のスラッシュで)に分割し, テキストに変換することを要求する. これにより, パターンマッチングが容易になる. もし, 元々の変換されない`ByteString`が必要であれば, `rawPathInfo`を用いることができる. 利用可能なフィールドに関するより深い情報に関しては, WAIハンドブックを参照しなさい. 

これによりリクエスト側が対処される; レスポンスについてはどうであろうか? すでに`responseLBS`については見ており, これは遅延`ByteString`からレスポンスを作るための便利な方法である. この関数は3つの引数を取る: ステータスコードと, レスポンスヘッダ(キー/値のペア)のリストと, ボディ自身である. しかし, `responseLBS`は単に都合上のラッパである. 表面下では. WAIはblaze-builderの`Builder`データ型を用いて, 生バイト列を表現する. 他のレベルを掘り下げ, それを直接使ってみよう:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder (Builder, fromByteString)
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, responseBuilder)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3000 app

app :: Application
app _req sendResponse = sendResponse $ responseBuilder
    status200
    [("Content-Type", "text/plain")]
```

これにより効率的にレスポンスボディを構築するための素晴らしい機会が開けてくる. なぜならば, `Builder`によって, O(1)の結合操作が可能になるためである. また, blaze-builderの頂点にあるblaze-htmlを, 活用することもできる. 


``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Network.HTTP.Types            (status200)
import           Network.Wai                   (Application, responseBuilder)
import           Network.Wai.Handler.Warp      (run)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import           Text.Blaze.Html5              (Html, docTypeHtml)
import qualified Text.Blaze.Html5              as H

main :: IO ()
main = run 3000 app

app :: Application
app _req sendResponse = sendResponse $ responseBuilder
    status200
    [("Content-Type", "text/html")] -- yay!
    (renderHtmlBuilder myPage)

myPage :: Html
myPage = docTypeHtml $ do
    H.head $ do
        H.title "Hello from blaze-html and Warp"
    H.body $ do
        H.h1 "Hello from blaze-html and Warp"
```

しかし, 純粋な`Builder`値を用いることに関しては制限がある: `Response`値を返す前に, 完全なレスポンスボディを構築する必要がある. 遅延評価により, それは言うほど悪いものではない. なぜならば, 全てのボディが一度にメモリに留まるわけではないためである. しかし, もし, I/Oを用いて, レスポンスボディを生成する必要があるならば(データベースからデータを読み込む際のように), 問題となるであろう. 

そのような状況に対処するために, WAIはストリーミングレスポンスボディを与えるための方法を提供する. また, それにより, ストリームフラッシュを明確にコントロールすることが可能になる. それがどのように機能するかについて, 見てみよう.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder           (Builder, fromByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import           Control.Concurrent                 (threadDelay)
import           Control.Monad                      (forM_)
import           Control.Monad.Trans.Class          (lift)
import           Data.Monoid                        ((<>))
import           Network.HTTP.Types                 (status200)
import           Network.Wai                        (Application,
                                                     responseStream)
import           Network.Wai.Handler.Warp           (run)

main :: IO ()
main = run 3000 app

app :: Application
app _req sendResponse = sendResponse $ responseStream
    status200
    [("Content-Type", "text/plain")]
    myStream

myStream :: (Builder -> IO ()) -> IO () -> IO ()
myStream send flush = do
    send $ fromByteString "Starting streaming response.\n"
    send $ fromByteString "Performing some I/O.\n"
    flush
    -- pretend we're performing some I/O
    threadDelay 1000000
    send $ fromByteString "I/O performed, here are some results.\n"
    forM_ [1..50 :: Int] $ \i -> do
        send $ fromByteString "Got the value: " <>
               fromShow i <>
               fromByteString "\n"

```

<p class = "info">
以前はwaiはconduitライブラリーに依存して, データ抽出のストリーミングを行なっていた. しかし, 以来, その依存性を解除している. しかし, conduitはwai-conduit helperパッケージにより, まだWAIのエコシステムにおいてよくサポートされている.
</p>

ストリーミングレスポンスを扱う際の, 他の共通に必要なものは, ファイルハンドルのようなわずかなリソースを安全に割り当てることである. "安全に"とは, 例外が起こった際でも, リソースが解放されることを意味する. これは, 上で述べた継続渡しスタイルが関与する場面である:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder (fromByteString)
import qualified Data.ByteString          as S
import           Data.Conduit             (Flush (Chunk), ($=))
import           Data.Conduit.Binary      (sourceHandle)
import qualified Data.Conduit.List        as CL
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, responseStream)
import           Network.Wai.Handler.Warp (run)
import           System.IO                (IOMode (ReadMode), withFile)

main :: IO ()
main = run 3000 app

app :: Application
app _req sendResponse = withFile "index.html" ReadMode $ \handle ->
    sendResponse $ responseStream
        status200
        [("Content-Type", "text/html")]
        $ \send _flush ->
            let loop = do
                    bs <- S.hGet handle 4096
                    if S.null bs
                        then return ()
                        else send (fromByteString bs) >> loop
             in loop
```

どのように`withFile`のような, もともと存在する例外安全関数を利用し, 例外を適切に処理しているかについて注意せよ. 

しかし, ファイルを与える際は`responseFile`を用いると, より効率的である. なぜならば, 不必要なバッファコピーを避けるために, `sendFile`システムコールが用いられるためである:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, responseFile)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3000 app

app :: Application
app _req sendResponse = sendResponse $ responseFile
    status200
    [("Content-Type", "text/html")]
    "index.html"
    Nothing -- means "serve whole file"
            -- you can also serve specific ranges in the file
```

WAIにはここでは扱わなかったような多くの特徴がある. 1つの重要な話題としてはWAIミドルウェアがある. また, リクエストボディについては全く調査しなかった. しかし, Yesodを理解するという目的に関しては, 今のところ十分に説明した. 

## Yesodについて

WAIやWarpについては楽しんだが, Yesodについてはまだ何も見ていない! ちょうどWAIについて学んだため, 最初の疑問点は次のようになるはずである: どのようにYesodはWAIと相互作用しているのであろうか? その答えは1つの非常に重要な関数にある:

`toWaiApp :: YesodDispatch site => site -> IO Application`

<p class = "info">
`toWaiAppPlain`と呼ばれるより基礎的な関数が存在する. 違いは, `toWaiAppPlain`は付加的なWAIミドルウェアをインストールしないが, `toWaiApp`はログ, GZIP圧縮やHEADリクエストメソッドハンドリングのような, 一般的に用いられるミドルウェアを与える.
</p>

この関数はサイト値を取るが, それは`YesodDispatch`のインスタンスである必要がある. そして, `Application`を構築する. この関数は`IO`モナドになる. なぜならば, それは共有のログバッファの割り当てのような操作を行うためである. より興味深い疑問点としては, `site`値が何であるかである. 

Yesodはファウンデーションデータ型という概念を持つ. これは各々のアプリケーションの中心にあるデータ型であり, 3つの重要な方法で用いられる. 

- それは, HTTPコネクションマネージャ, データベースコネクションプール, ファイルからロードされたセッティングや, カウンタやキャッシュのような大域的な可変状態などの初期化され, アプリケーション全体に渡り共有される値を保持する.

- 型クラスのインスタンスは, アプリケーションに関するより多くの情報を与える. `Yesod`型クラスは, デフォルトのテンプレートがどのようであるかや, 許容される最大のリクエストボディサイズのような, 様々な設定を持つ. `YesodDispatch`クラスはどのように, 入ってくるリクエストがハンドラ関数にディスパッチされるかについて, 指示する. そして, Yesodヘルパライブラリには, i18nサポートのための`RenderMessage`や, jQuery Javascriptライブラリの共通の場所を与える`YesodJquery`のような, 多くの共通に用いられる型クラスが存在する. 

- 関連型(タイプファミリのような)は, 各アプリケーションに対し, 関連するルートデータ型を作るために用いられる. これは, アプリケーションにおける全ての合法的なルートを表す, 単純なADTである. しかし, 直接文字列を用いる代わりに, この中間データ型を用いることで, Yesodアプリケーションはコンパイラを利用し, 無効なリンクを防ぐことができる. この特徴は, 型安全URLと呼ばれる. 

この章における考え方に沿って, 最初のYesodアプリケーションを全てを手動で書くことで, 苦労して作ってみる. 進むにつれ, より便利なヘルパを順次追加していく.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
import           Network.HTTP.Types            (status200)
import           Network.Wai                   (responseBuilder)
import           Network.Wai.Handler.Warp      (run)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import qualified Text.Blaze.Html5              as H
import           Yesod.Core                    (Html, RenderRoute (..), Yesod,
                                                YesodDispatch (..), toWaiApp)
import           Yesod.Core.Types              (YesodRunnerEnv (..))

-- | Our foundation datatype.
data App = App
    { welcomeMessage :: !Html
    }

instance Yesod App

instance RenderRoute App where
    data Route App = HomeR -- just one accepted URL
        deriving (Show, Read, Eq, Ord)

    renderRoute HomeR = ( [] -- empty path info, means "/"
                        , [] -- empty query string
                        )

instance YesodDispatch App where
    yesodDispatch (YesodRunnerEnv _logger site _sessionBackend _ _) _req sendResponse =
        sendResponse $ responseBuilder
            status200
            [("Content-Type", "text/html")]
            (renderHtmlBuilder $ welcomeMessage site)

main :: IO ()
main = do
    -- We could get this message from a file instead if we wanted.
    let welcome = H.p "Welcome to Yesod!"
    waiApp <- toWaiApp App
        { welcomeMessage = welcome
        }
    run 3000 waiApp
```

さあ, かなり多くの新しいものを追加したが, 一度にそれらを見てみよう. まず最初に, 新しいデータ型である`App`を構築した. これは, 各アプリケーションにおいて, ファウンデーションデータ型の名前として共通に用いられるが, どんな名前でも好きなものを用いてよい. このデータ型に1つのフィールドを追加した. それは, `welcomeMessage`でありホームページにおけるコンテンツを持っている.

次に, Yesodインスタンスを宣言する. この例に関しては, 全てのメソッドにおいてデフォルト値を用いる. より興味深いのは, `RenderRoute`型クラスである. これは型安全URLの中心部である. `App`に関する関連データ型を作る. それらは, アプリケーションにおける全ての許容されたルートを一覧にする. この場合, 1つだけ追加した: それは`HomeR`と呼ぶホームページである. 全てのルートデータコンストラクタに`R`を付加することは, Yesodにおける名称をつける際の慣習である.

また, `renderRoute`メソッドを作る必要がある. それは, 各型安全ルート値をパスピースと, クエリストリングパラメータのタプルに変換する. 後に, より興味深い例を挙げるが, 今の段階では, それら両方については空リストになっている. 

`YesodDispatch`はアプリケーションの振る舞いを決定する. それは`YesodDispatch`と呼ばれる1つのメソッドを持つ:

``` haskell
yesodDispatch :: YesodRunnerEnv site -> Application
```

`YesodRunnerEnv`は3つの値を与える: ログメッセージを出力するための`Logger`値, ファウンデーションデータ型それ自身, ユーザのアクティブセッションにおいて情報を格納したり, 取得したりするためのセッションバックエンド. 実際のYesodアプリケーションにおいては, すぐにわかるように, これらの値を直接扱う必要はない. しかし, 表面下に何があるかを理解することは有益である. 

`YesodDispatch`の戻り値の型はWAIからの`Application`である. しかし最初の方で見たように, `Application`は単なる`Request`から`Response`へのCPSed(継続渡しスタイル)関数である. 従って, `YesodDispatch`の実装については, 上のWAIで学んだことを使うことができる. どのようにファウンデーションデータ型から`welcomeMessage`にアクセスするかについても注意せよ. 

最後に, `main`関数がある. `App`値は作るのが容易であり, お分かりのように, ウェルカムメッセージを得るためにI/O操作を同じくらい容易にできたはずである. `toWaiApp`を用いて, WAIアプリケーションを得て, 前に行ったようにアプリケーションをワープに渡す. 

おめでとうございます, 最初のYesodアプリケーションを作ることができました!(あるいは, 少なくともこの章における最初のアプリケーションである.)

## HandlerT monad transformer

その例はYesodを技巧的に使っているが, 信じがたいほどに刺激性のないものである. WAIに関しては, Yesodは邪魔になる以外, 何も行っていないことに関する疑念はない. それは, Yesodの機能を何も利用し始めていないためである. `HandlerT`モナドトランスフォーマから初めて, それに対処しよう. 

単一のリクエストを処理する際に, 一般的に行いたいことが多く存在する, 例えば:

- 何らかのHTMLを返す.

- 異なるURLにリダイレクトする.

- 404 not foundレスポンスを返す.

- ログを取る.

これら共通の機能全てをカプセル化するために, Yesodは`HandlerT`モナドトランスフォーマを与える. Yesodにおけるコードの大部分は, このトランスフォーマに存在するため, それには精通すべきである. 前の例における`YesodDispatch`インスタンスを`HandlerT`を利用する新しいインスタンスで置き換えるところから始めよう:

``` haskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
import           Network.Wai              (pathInfo)
import           Network.Wai.Handler.Warp (run)
import qualified Text.Blaze.Html5         as H
import           Yesod.Core               (HandlerT, Html, RenderRoute (..),
                                           Yesod, YesodDispatch (..), getYesod,
                                           notFound, toWaiApp, yesodRunner)

-- | Our foundation datatype.
data App = App
    { welcomeMessage :: !Html
    }

instance Yesod App

instance RenderRoute App where
    data Route App = HomeR -- just one accepted URL
        deriving (Show, Read, Eq, Ord)

    renderRoute HomeR = ( [] -- empty path info, means "/"
                        , [] -- empty query string
                        )

getHomeR :: HandlerT App IO Html
getHomeR = do
    site <- getYesod
    return $ welcomeMessage site

instance YesodDispatch App where
    yesodDispatch yesodRunnerEnv req sendResponse =
        let maybeRoute =
                case pathInfo req of
                    [] -> Just HomeR
                    _  -> Nothing
            handler =
                case maybeRoute of
                    Nothing -> notFound
                    Just HomeR -> getHomeR
         in yesodRunner handler yesodRunnerEnv maybeRoute req sendResponse

main :: IO ()
main = do
    -- We could get this message from a file instead if we wanted.
    let welcome = H.p "Welcome to Yesod!"
    waiApp <- toWaiApp App
        { welcomeMessage = welcome
        }
    run 3000 waiApp

```

`getHomeR`は最初のハンドラ関数である. (その名前はYesodの世界におけるさらに他の名称慣習である: 小文字のHTTPリクエストメソッドに続き, ルートコンストラクタ名がある.) その型注釈に注意しなさい: `HandlerT App IO Html`. `HandlerT App IO`モナドスタックを持つことは非常に一般的であるため, 大部分のアプリケーションはその型シノニムを持つ. `type Handler = HandlerT App IO`. その関数は`Html`を返す. Yesodは`Html`値に対してのみ機能するようにハードコードされているのか, 疑問に思うかもしれない. すぐにそれについての詳細を説明する. 

関数の中身は端的である. `getYesod`関数を用いて, ファウンデーションデータ値を得て, `welcomeMessage`フィールドを返す. 進行するにつれ, より興味深いハンドラを構築する. 

`YesodDispatch`の実装は今や全く異なったものである. そのための鍵となるのは, `yesodRunner`関数であり, それは, `HandlerT`スタックをWAI `Application`に変換するための, ローレベル関数である. その型注釈を見てみよう:

``` haskell

yesodRunner :: (ToTypedContent res, Yesod site)
            => HandlerT site IO res
            -> YesodRunnerEnv site
            -> Maybe (Route site)
            -> Application

```

前の例からすでに`YesodRunnerEnv`には精通している. 上の`yesodRunner`の呼び出しでもわかる通り, その値は変更せずに渡される. `Maybe (Route site)`は多少興味深く, どのように型安全URLが機能するかについてより洞察を与える. 今まで, これらのURLのレンダリング側のみ見てきた. しかし, パーズ側も同様に重要である: リクエストされたパスをルート値に変換する. 例においては, このコードは数行に過ぎず, 結果を`maybeRoute`に格納している.

<p class = "info">
現在のパーズ関数が小さいことは確かであるが, 大きなアプリケーションにおいてはより複雑である必要があり, 動的パラメータのような問題も扱う. その点においては, パーズ, レンダリング関数が適切な配置にあることを保証するのは, 些細でない試みである. 後にYesodがそれをどのように対処するかについて学ぶ. 
</p>

`Yesod Runner`へのパラメータに戻ろう: つい先ほどは`Maybe (Route site)`と`YesodRunnerEnv site`を扱った. `HandlerT site IO res`値を得るために, `maybeRoute`でパターンマッチを行う. もし, `Just HomeR`の場合, `getHomeR`を使う. その他の場合, `notFound`関数を用いる. これは, 404 not foundレスポンスを返す内蔵の関数であり, デフォルトのサイトテンプレートを用いる. テンプレートはYesod型クラスでオーバーライド可能である; それは単なるつまらないHTMLページである. 

1つの例外を除き, 全て意味を成している: `ToTypedContent`型クラスは何であり, `Html`レスポンスとどんな関係があるのであろうか? 上の質問から答えることにしよう: いいえ, Yesodは決して`Html`に対しハードコードサポートしている訳ではない. ハンドラ関数は`ToTypedContent`インスタンスを持つどんな値でも返すことができる. この型クラス(すぐに調べるであろう)は, mime型と, WAIが消費できるデータの表現の両者を与える. `yesodRunner`はそれをWAIレスポンスに変換し, `Content-type`レスポンスヘッダをmime型にセットし, 200 OKステータスコードを用いて, レスポンスボディを送信する. 

### (To)Content, (To)TypedContent

Yesodのコンテンツシステムの中心部には次の型が存在する:

``` haskell
data Content = ContentBuilder !Builder !(Maybe Int) -- ^ The content and optional content length.
             | ContentSource !(Source (ResourceT IO) (Flush Builder))
             | ContentFile !FilePath !(Maybe FilePart)
             | ContentDontEvaluate !Content

type ContentType = ByteString
data TypedContent = TypedContent !ContentType !Content
```

`Content`はWAIレスポンス型を少し思い出させるであろう. `ContentBuilder`は`ResponseBuilder`と類似しており, `ContentSource`は`responseStream`に似ているが, conduitに特化しており, `ContentFile`は`responseFile`に類似している. WAIの対応物と異なり, これらのどのコンストラクタもステータスコードやレスポインスヘッダについての情報を含まない; それはYesodにおいては, 別で扱われる.

1つの完全に新しいコンストラクタは, `ContentDontEvaluate`である. デフォルトでは, Yesodにおいてレスポンスボディを作る際, Yesodはレスポンスを生成する前に, ボディを完全に評価する. この理由としては, 値に純粋でないエラーが存在しないことを保証するためである. Yesodはレスポンスを送り始める前に, このような例外をキャッチすることを確実にしようとしている. そのため, もし例外が発生した場合, エラーでないレスポンスを送る途上で落ちてしまう代わりに, 適切な500 internal serverエラーを生成することができる. しかし, この評価を行うにはよりメモリを必要とする. したがって, Yesodはこの保護から脱退する方法を提供する. 

`TypedContent`は`Content`に多少の追加を行ったものである: それは, 同様に`ContentType`を含む. アプリケーションは指定されなければ200 OKステータスを返すという慣習と共に, `TypedContentType`型からレスポンスを生成するために必要な全てのものを取得できる. 

Yesodはユーザに常に`TypedContent`を, ハンドラ関数から返すことを要求する方法を取れ得たかもしれない. しかし, それは手動でその型に変換することを要したであろう. 代わりに, Yesodはこのために, `ToContent`と`ToTypedContent`という適切に命名された2つの型クラスを用いる. それらは, 正に予想通りの定義である.

``` haskell
class ToContent a where
    toContent :: a -> Content
class ToContent a => ToTypedContent a where
    toTypedContent :: a -> TypedContent
```

そして, Yesodは多くの一般的に用いられるデータ型に対し, インスタンスを与え, `Text`, `Html`, そして, aesonの`Value`型(JSONデータを含む)が含まれる. そのようにして, `getHomeR`関数は`Html`を返すことができる: Yesodはそれをどのように`TypedContent`に変換するかを知っており, そこから`WAI`レスポンスに変換される. 

### HasContentTypeと表現

この型クラスによる方法は, 1つの新しいよい抽象化を与える. 多くの型の場合, 型システム自身は, コンテンツのコンテンツ型が何であるべきかを知らせる. 例えば, `Html`は
常に, `text/html`コンテンツ型で扱われる. 

<p class = "info">
これは`ToTypedContent`の全てのインスタンスに対し正しい訳ではない. 反例として, `ToTypedContent TypedContent`インスタンスを考えてみよ.
</p>

ウェブアプリケーションに対するリクエストのいくつかは, 様々な表現で表示される. 例えば, 表形式データのリクエストは次のように扱われる:

- HTMLテーブル

- CSVファイル

- XML

- クライアント側のより消費されるJSONデータ

HTTP使用によりクライアントは`accept`リクエストヘッダにより表現の嗜好を指定することが可能である. そして, Yesodによりハンドラ関数が`selectRep`/`provideRep`関数の組み合わせを用いて, 複数の表現を与え, フレームワークにクライアントヘッダに基づき, 自動的に適切なものを選ばせることが可能になる. 

これらを全て機能させるために, 最後に残ったものは, `HasContentType`型クラスである:

``` haskell
class ToTypedContent a => HasContentType a where
    getContentType :: Monad m => m a -> ContentType
```

パラメータ`m a`は, 単なる`Proxy`型である. 後から考えると`Proxy`を用いるべきだったが, 今や互換性を破る変更でしかない. この型クラスには, `ToTypedContent`によりサポートされている大部分の型に対しインスタンスが存在する. 以下では, データの複数の表現を与えるため多少ひねって, 上の例を挙げる:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text                (Text)
import           Network.Wai              (pathInfo)
import           Network.Wai.Handler.Warp (run)
import qualified Text.Blaze.Html5         as H
import           Yesod.Core               (HandlerT, Html, RenderRoute (..),
                                           TypedContent, Value, Yesod,
                                           YesodDispatch (..), getYesod,
                                           notFound, object, provideRep,
                                           selectRep, toWaiApp, yesodRunner,
                                           (.=))

-- | Our foundation datatype.
data App = App
    { welcomeMessageHtml :: !Html
    , welcomeMessageText :: !Text
    , welcomeMessageJson :: !Value
    }

instance Yesod App

instance RenderRoute App where
    data Route App = HomeR -- just one accepted URL
        deriving (Show, Read, Eq, Ord)

    renderRoute HomeR = ( [] -- empty path info, means "/"
                        , [] -- empty query string
                        )

getHomeR :: HandlerT App IO TypedContent
getHomeR = do
    site <- getYesod
    selectRep $ do
        provideRep $ return $ welcomeMessageHtml site
        provideRep $ return $ welcomeMessageText site
        provideRep $ return $ welcomeMessageJson site

instance YesodDispatch App where
    yesodDispatch yesodRunnerEnv req sendResponse =
        let maybeRoute =
                case pathInfo req of
                    [] -> Just HomeR
                    _  -> Nothing
            handler =
                case maybeRoute of
                    Nothing -> notFound
                    Just HomeR -> getHomeR
         in yesodRunner handler yesodRunnerEnv maybeRoute req sendResponse

main :: IO ()
main = do
    waiApp <- toWaiApp App
        { welcomeMessageHtml = H.p "Welcome to Yesod!"
        , welcomeMessageText = "Welcome to Yesod!"
        , welcomeMessageJson = object ["msg" .= ("Welcome to Yesod!" :: Text)]
        }
    run 3000 waiApp
```

### 便利なwarp関数

Yesodの世界においてかなり見かける1つの小さな便利な関数. `toWaiApp`を呼び, WAI `Application`を作り, それをWarpの`run`関数に渡すことは非常い一般的である. よって, Yesodは便利な`warp`ラッパー関数を与える. 前の例における`main`関数を次で置き換えることができる:

``` haskell
main :: IO ()
main =
    warp 3000 App
        { welcomeMessageHtml = H.p "Welcome to Yesod!"
        , welcomeMessageText = "Welcome to Yesod!"
        , welcomeMessageJson = object ["msg" .= ("Welcome to Yesod!" :: Text)]
        }
```

`warpEnd`関数もあり, これは, ポートナンバを`PORT`環境変数から読み取る. これは, FP Haskell Centerや. Keterなどのデプロイメントツールのようなプラットフォームで作業する場合に有用である. 

## ハンドラを書く

大部分のアプリケーションは`HandlerT`モナドトランスフォーマに存在することになるため, その文脈において機能するかなり多くの関数が存在することは驚くべきことではない. `HandlerT`は多くの共通の型クラスのインスタンスであり, `MonadIO`, `MonadTrans`, `MonadLogger`, そして, `MonadResource`を含み, その結果自動的にそれらの機能を利用することができる. 

その標準的機能の他に, 次に挙げるのは関数の一般的なカテゴリである. Yesodがハンドラ関数に置く唯一の要求は, 最終的にそれらが, `ToTypedContent`のインスタンスである型を返すことである. 

この章は機能の端的な概略に過ぎない, 詳細については, Haddockかこの本の残りを調べるべきである.

### リクエストパラメータを得る

リクエストの中でクライアントにより与えられるいくつかの情報がある:

- リクエストされるパス. これは大抵Yesodのルーティングフレームワークにより処理され, ハンドラ関数に直接問い合わせられる訳ではない. 

- クエリ文字列パラメータ. これは, `lookupGetParam`により問い合わせられる. 

- リクエストボディ. URLエンコードされたmultipartボディの場合, リクエストパラメータを得るために`lookupPostParam`を用いることができる. multipleボディについては, ファイルパラメータに対し, `lookupFile`が存在する. 

- リクエストヘッダは`lookupHeader`を用いて問い合わせられる. (そして, レスポンスヘッダは`addHeader`によりセットされる).

- Yesodは自動的にクッキをパーズし, `lookUpCookie`を用いて問い合わせられる. (クッキは`setCookie`によりセットされる.)

- 最後に, Yesodはユーザセッションフレームワークを与える. そこでは, データは暗号的に安全なセッションに
セットされ, 各々のユーザと関連づけられる. これは`lookupSession`, `setSession`, そして`deleteSession`を用いて問い合わせられ, セットされる.

フォームを処理するような目的のために, 直接これらの関数を用いることができるが, yesod-formライブラリを用いたいと思うであろう. それは, applicativeフォームに基づいた高レベルの抽象化フォームを与える. 

### short circuiting

リクエストの処理をショートカットしたい場合があるであろう. これを行う理由としては以下のようになる:

- `redirect`関数を用いて. HTTPリダイレクトを送る. これはデフォルトで303ステータスコードを用いることになる. これをより制御したい場合, `redirectWith`を用いることができる. 

- `notFound`では, 404 not foundを, `badMethod`では405 bad methodを返す.

- `notAuthenticated`, `permissionDenied`, または, `invalidArgs`によりリクエストにおけるエラーを指摘する.

- `sendFile`や`sendResponseStatus`(status 200レスポンスコードをオーバーライドするために)で, 特別なレスポンスを送る.

- 抽象化のレベルを落とし, Yesodによる抽象化をバイパスして, WAI自身を用いるために, `sendWaiResponse`を使う. 

### streaming

今までのところ, 挙げられた`ToTypedContent`インスタンスの例は全て, ストリーミングでないレスポンスに関連するものであった. `Html`, `Text`, そして, `Value`は全て`ContentBuilder`コンストラクタに変換される. そのため, それらはデータをユーザに送る点で, I/Oと交互作用しない. そのような交互作用を行いたい場合, 何が生じるであろうか.

WAIにおいてこの問題に遭遇した場合, レスポンスを構築するために`responseSource`メソッドを導入した. `sendWaiResponse`を用いることで, Yesodにおいてストレーミングレスポンスを作るために, 同じメソッドを再利用できる. しかし, これを行うためのより単純なAPIが存在する: `respondSource`. `respondSource`は, 2つのパラメータを取る: レスポンスのコンテンツ型と, `Flush Builder`の`Source`である. Yesodはまた, `sendChunk`, `sendChunkBS`, そして, `sendChunkText`のような`Source`を作るためのいくつもの便利な関数を提供する.  

ここに例を挙げる. ここでは, 最初の`responseSource`の例をWAIからYesodに変換する. 

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
import           Blaze.ByteString.Builder           (fromByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import           Control.Concurrent                 (threadDelay)
import           Control.Monad                      (forM_)
import           Data.Monoid                        ((<>))
import           Network.Wai                        (pathInfo)
import           Yesod.Core                         (HandlerT, RenderRoute (..),
                                                     TypedContent, Yesod,
                                                     YesodDispatch (..), liftIO,
                                                     notFound, respondSource,
                                                     sendChunk, sendChunkBS,
                                                     sendChunkText, sendFlush,
                                                     warp, yesodRunner)

-- | Our foundation datatype.
data App = App

instance Yesod App

instance RenderRoute App where
    data Route App = HomeR -- just one accepted URL
        deriving (Show, Read, Eq, Ord)

    renderRoute HomeR = ( [] -- empty path info, means "/"
                        , [] -- empty query string
                        )

getHomeR :: HandlerT App IO TypedContent
getHomeR = respondSource "text/plain" $ do
    sendChunkBS "Starting streaming response.\n"
    sendChunkText "Performing some I/O.\n"
    sendFlush
    -- pretend we're performing some I/O
    liftIO $ threadDelay 1000000
    sendChunkBS "I/O performed, here are some results.\n"
    forM_ [1..50 :: Int] $ \i -> do
        sendChunk $ fromByteString "Got the value: " <>
                    fromShow i <>
                    fromByteString "\n"

instance YesodDispatch App where
    yesodDispatch yesodRunnerEnv req sendResponse =
        let maybeRoute =
                case pathInfo req of
                    [] -> Just HomeR
                    _  -> Nothing
            handler =
                case maybeRoute of
                    Nothing -> notFound
                    Just HomeR -> getHomeR
         in yesodRunner handler yesodRunnerEnv maybeRoute req sendResponse

main :: IO ()
main = warp 3000 App
```

##　動的パラメータ

さあ, `HandlerT`トランスフォーマの詳細についての回り道は終了したため, より高レベルのYesodリクエスト処理に戻ろう. 今までのところ, 全ての例は単一でサポートされたリクエストルートを扱ってきた. これをより楽しいものにしよう. これから, Fibonacci数を扱うアプリケーションを作りたい. もし, `/fib/5`へリクエストする場合, 5番目のFibonacci数が返ってくる. もし, `/`を訪問する場合, 自動的に`/fib/1`にリダイレクトする.

Yesodの世界において, 最初に尋ねるべき質問としては: どのようにルートデータ型をモデルかするか? これは非常に率直である: `data Route App = HomeR | FibR Int`. 質問としては: どのように`RenderRoute`インスタンスを定義したいか? `Int`を`Text`に変換する必要がある. どの関数を使うべきであろうか?

それについて答える前に, ディスパッチ目的のため`Text`を`Int`にパーズして戻す必要があることを理解しなさい. したがって, `fromText . toText == Just`という性質を持つような, 関数のペアを持っていることを確認する必要がある. `Show`/`Read`は次を除いて, その候補となる:

- `String`を通し変換する必要がある.

- `Text`の`Show`/`Read`インスタンスは両方とも, 望まぬ余計なエスケープを含む.

代わりに, Yesodによってとられる方法は, path-pieceパッケージであり, 特に`PathPiece`型クラスは次のように定義される:


``` haskell
class PathPiece s where
    fromPathPiece :: Text -> Maybe s
    toPathPiece   :: s    -> Text
```

この型クラスを使うことで, ルートデータ型のためのパーズ, レンダリング関数を書くことができる. 

``` haskell
instance RenderRoute App where
    data Route App = HomeR | FibR Int
        deriving (Show, Read, Eq, Ord)

    renderRoute HomeR = ([], [])
    renderRoute (FibR i) = (["fib", toPathPiece i], [])

parseRoute' [] = Just HomeR
parseRoute' ["fib", i] = FibR <$> fromPathPiece i
parseRoute' _ = Nothing
```

そして, `YesodDispatch`型クラスのインスタンスを書く.

``` haskell
instance YesodDispatch App where
    yesodDispatch yesodRunnerEnv req sendResponse =
        let maybeRoute = parseRoute' (pathInfo req)
            handler =
                case maybeRoute of
                    Nothing -> notFound
                    Just HomeR -> getHomeR
                    Just (FibR i) -> getFibR i
         in yesodRunner handler yesodRunnerEnv maybeRoute req sendResponse

getHomeR = redirect (FibR 1)

fibs :: [Int]
fibs = 0 : scanl (+) 1 fibs

getFibR i = return $ show $ fibs !! i
```

`getHomeR`における`redirect`呼び出しに注意しなさい. `redirect`のパラメータとしてルートデータ型を使うことができ, Yesodは`renderRoute`関数を利用して, テキストリンクを作る.

## Template Haskellを用いたルーティング

前のアプリケーションに新しいルートを追加したいとしよう. 次のような変更をする必要がある:

1. `Route`データ型自身を変更する.

2. `renderRoute`に節を追加する.

3. `parseRoute'`に節を追加し, `renderRoute`と適切に対応することを確認する.

4. `YesodDispatch`におけるcase文に, ハンドラ関数を呼ぶための節を追加する.

5. ハンドラ関数を書く.

かなり多くの変更がある! 多くの手動, ビウラープレート変更は誤りにつながる可能性が大きい. 警告をオンにしていれば, コンパイラーによって捉えられる誤りもあるが(`renderRoute`の節や, `YesodDispatch`のcase構文におけるパターンマッチを追加し忘れた場合), そうでないエラーもある(`renderRoute`と`parseRoute`が同じロジックを持っていることを確認することや, `parseRoute`の節を追加すること).

ここは, Template HaskellのYesodの世界における出番である. これら全ての変化を手動で扱う代わりに, Yesodは高レベルのルーティング構文を宣言する. この宣言によりルート構文, 動的パラメータ, コンストラクタ名, そして, 許容されるリクエストメソッドを指定し, 自動的にパーズ, レンダリング, ディスパッチ関数を生成することが可能になる.  

どのくらいの手動コーディングを省略できるかについて知るために, 前の例がTemplate Haskell版に変更されたものを見てみよう.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import           Yesod.Core (RenderRoute (..), Yesod, mkYesod, parseRoutes,
                             redirect, warp)

-- | Our foundation datatype.
data App = App

instance Yesod App

mkYesod "App" [parseRoutes|
/         HomeR GET
/fib/#Int FibR  GET
|]

getHomeR :: Handler ()
getHomeR = redirect (FibR 1)

fibs :: [Int]
fibs = 0 : scanl (+) 1 fibs

getFibR :: Int -> Handler String
getFibR i = return $ show $ fibs !! i

main :: IO ()
main = warp 3000 App
```

これについて素晴らしいことは, 開発者としてアプリケーションの重要な部分に集中でき, パーサやレンダリング関数などの詳細に関与せずに済むことである. もちろん, Template Haskellを使うことの欠点も存在する:

- コンパイル時間が少し遅い.

- 舞台裏で起こっていることの詳細が, あまり明瞭ではない. (どのような識別子が生成されているかを見るために, `cabal haddock`を用いることができるが)

- あまり詳細に制御することができない. 例えば, Yesodのルート構文において, 各動的パラメータは, 結束したフィールドではなく, ルートコンストラクタと分離したフィールドである必要がある. これは, Yesodにおける柔軟性と複雑性の間の意識的なトレードオフである.

Template Haskellの使用は, Yesodにおける最も論争的な決断である. 個人的には, その利点は明らかに仕様を正当化すると思っている. しかし, Template Haskellを避けたいと思うならば, 自由にそうすることができる. 今までのところ全ての例はそのようにしており, それらの技術に倣うことができる. Yesodの世界には他のより単純な方法がある: `LiteApp`.

## LiteApp

`LiteApp`により, 型安全URLやTemplate Haskellを使わないで済ませることができる. それは, 純粋Haskellにおける単純なルーティングDSLを用いる. 再び, 単純な比較として, FIbonacci例をそれを使って書き直してみよう. 

``` haskell
import           Data.Text  (pack)
import           Yesod.Core (LiteHandler, dispatchTo, dispatchTo, liteApp,
                             onStatic, redirect, warp, withDynamic)

getHomeR :: LiteHandler ()
getHomeR = redirect "/fib/1"

fibs :: [Int]
fibs = 0 : scanl (+) 1 fibs

getFibR :: Int -> LiteHandler String
getFibR i = return $ show $ fibs !! i

main :: IO ()
main = warp 3000 $ liteApp $ do
    dispatchTo getHomeR
    onStatic (pack "fib") $ withDynamic $ \i -> dispatchTo (getFibR i)
```

全く言語拡張のない, 単純なYesodアプリケーションである! しかhし, このアプリケーションでさえ, 多少の安全性を示す. Yesodは`getFibR`のパラメータを`Text`から`Int`に変換するために, `fromPathPiece`を用いる. したがって, 不適切なパラメータは全てYesod自身により捕えられる. 1つのチェックだけ行わないで済ませることができる.

## Shakespeare

単純なテキストページを生成することは楽しいかもしれないが, これは通常ウェブフレームワークに期待するものとは言い難い. 望むように, YesodはHTML, CSS, そして, Javascriptを生成する内蔵のサポートがあるように作られている. 

template言語に入る前に, それを生で, すなわちローレベルの方法で行い, それからもう少し楽しいものを作ろう.

``` haskell
import           Data.Text  (pack)
import           Yesod.Core

getHomeR :: LiteHandler TypedContent
getHomeR = return $ TypedContent typeHtml $ toContent
    "<html><head><title>Hi There!</title>\
    \<link rel='stylesheet' href='/style.css'>\
    \<script src='/script.js'></script></head>\
    \<body><h1>Hello World!</h1></body></html>"

getStyleR :: LiteHandler TypedContent
getStyleR = return $ TypedContent typeCss $ toContent
    "h1 { color: red }"

getScriptR :: LiteHandler TypedContent
getScriptR = return $ TypedContent typeJavascript $ toContent
    "alert('Yay, Javascript works too!');"

main :: IO ()
main = warp 3000 $ liteApp $ do
    dispatchTo getHomeR
    onStatic (pack "style.css") $ dispatchTo getStyleR
    onStatic (pack "script.js") $ dispatchTo getScriptR
```

すでに学んだ`TypedContent`の素材を再利用しているに過ぎない. 今回は, 3つの分離したルートがあり, HTML, CSS, そして, JavaScriptを提供している. コンテンツを`String`として与え, `toContent`を用いて, それらを`Content`に変換し, `TypedContent`コンストラクタによりラップし, 適切なコンテンツ型ヘッダを与えている.

しかしいつものように, よりよくすることができる. `String`を扱うことはあまり効率的ではなく, 常に手動でコンテンツ型を入力しなければならないのは面倒なことである. しかし, それらの問題への解決策をすでに知っている: `blaze-html`の`Html`データ型を使うことである. それを使うために, `getHomeR`関数を変更してみよう:

``` haskell

import           Data.Text                   (pack)
import           Text.Blaze.Html5            (toValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Yesod.Core

getHomeR :: LiteHandler Html
getHomeR = return $ H.docTypeHtml $ do
    H.head $ do
        H.title $ toHtml "Hi There!"
        H.link ! A.rel (toValue "stylesheet") ! A.href (toValue "/style.css")
        H.script ! A.src (toValue "/script.js") $ return ()
    H.body $ do
        H.h1 $ toHtml "Hello World!"

getStyleR :: LiteHandler TypedContent
getStyleR = return $ TypedContent typeCss $ toContent
    "h1 { color: red }"

getScriptR :: LiteHandler TypedContent
getScriptR = return $ TypedContent typeJavascript $ toContent
    "alert('Yay, Javascript works too!');"

main :: IO ()
main = warp 3000 $ liteApp $ do
    dispatchTo getHomeR
    onStatic (pack "style.css") $ dispatchTo getStyleR
    onStatic (pack "script.js") $ dispatchTo getScriptR

```

ずっと, 素晴らしい. `blaze-html`は便利なコンビネータライブラリを提供し, ほとんどの場合, 行おうとするどんな`String`連結よりも早く実行できる. 

もし, `blaze-html`に満足であれば, 是非それらを使いなさい. しかし, 多くの人々はより特化したテンプレート言語を使いたいと思うであろう. このためのYesodの標準的提供元はShakespear言語である: Hamlet, Lucius, そして, Julius. もし望むなら別のシステムを使っても全く問題ないが, 唯一必要なものは`Content`値をテンプレートから得られることである. 

Shakespearテンプレートは, コンパイル時にチェックされるため, それらを使うにはquasiquoteまたは, Template Haskellが必要になる. ここでは, 前者の方法を用いる. より詳細については, Shakespearの章を参照してください. 

``` haskell
{-# LANGUAGE QuasiQuotes #-}
import           Data.Text   (Text, pack)
import           Text.Julius (Javascript)
import           Text.Lucius (Css)
import           Yesod.Core

getHomeR :: LiteHandler Html
getHomeR = withUrlRenderer $
    [hamlet|
        $doctype 5
        <html>
            <head>
                <title>Hi There!
                <link rel=stylesheet href=/style.css>
                <script src=/script.js>
            <body>
                <h1>Hello World!
    |]

getStyleR :: LiteHandler Css
getStyleR = withUrlRenderer [lucius|h1 { color: red }|]

getScriptR :: LiteHandler Javascript
getScriptR = withUrlRenderer [julius|alert('Yay, Javascript works too!');|]

main :: IO ()
main = warp 3000 $ liteApp $ do
    dispatchTo getHomeR
    onStatic (pack "style.css") $ dispatchTo getStyleR
    onStatic (pack "script.js") $ dispatchTo getScriptR

``` 

### URLレンダリング関数

おそらくこれの最も混乱する部分は, `withUrlRenderer`の呼び出しである. これは, Yesodにおける最も強力な特徴の1つに踏み込む: 型安全URLである. もしHTMLにおいて注意するならば, CSSとJavascript URLへのリンクを文字列で与えている点である. `main`関数においてもそれらの文字列を2回目に与える必要があるため, これは情報の重複につながる. これはとても脆弱である: コードは壊れたリンクの修正を1つ多く行う必要がある.

代わりに推奨される方法としては, テンプレートにおいて明確な文字列を含む代わりに, 型安全URLデータ型を用いることである. 上で言及されたように, `LiteApp`は意味のある型安全URLを全く提供しないため, ここではその選択肢はない. しかし, Template Haskell生成器を用いるならば, 自由に型安全URLを得ることができる. 

どんな場合においても, Shakespearテンプレートは全て, 型安全URLのレンダリングを行う関数を受け取る必要がある. 型安全URLは実際に使われていないため, ここでは, ほぼどんな関数でも機能する(その関数は完全に無視される). しかhし, `withUrlRenderer`はこれを行うための便利な方法である. 

次に見るように, `withUrlRenderer`は実際, 大部分の場合必要とならない. なぜなら, Widgetが最終的に, 自動的にレンダリング関数を提供するためである.

### Widget

HTML, CSS, そして, Javascriptを個々の要素として扱うことは, 多くの場合便利である. しかし, ページに対し再利用可能な要素を構築したい場合, それは構成において邪魔になる. もし, なぜwidgetが便利なのかについてより深い動機を求めるならば, widgetの章を見てください. 今は, それらを使うことについて掘り下げてみましょう.

``` haskell
{-# LANGUAGE QuasiQuotes #-}
import           Yesod.Core

getHomeR :: LiteHandler Html
getHomeR = defaultLayout $ do
    setTitle $ toHtml "Hi There!"
    [whamlet|<h1>Hello World!|]
    toWidget [lucius|h1 { color: red }|]
    toWidget [julius|alert('Yay, Javascript works too!');|]

main :: IO ()
main = warp 3000 $ liteApp $ dispatchTo getHomeR
```
これは上と同じ例であるが, 今回は1つのハンドラに凝縮している. Yesodは自動的にCSSとJavascriptをHTMLに与えるような操作を行う. デフォルトでは, それぞれ`style`と`script`タグによりページの`head`と`body`に置く. しかし, Yesodは多くのカスタマイズ設定を与え, 他のこともできるようにする(自動的に一時的な静的ファイルを生成し, それらにリンクするような). 

widgetには他の利点もある. `defaultLayout`関数は, Yesod型クラスのメンバであり, ウェブサイトに対し, カスタマイズされた見た目を与えるように変更可能である. エラーメッセージのような, 多くのYesodにおける内蔵の機能は, widgetシステムを利用している. したがって, widgetを使うことで, 全てのサイトにおいて一貫した感じ方を得ることができる. 

## 扱わない詳細

幸いなことに, この章は表面化において何が起こっているかについて理解するために, Yesodにおける魔法を充分引っ張ってこれた. もちろんこの方法を使いながら, Yesodの生態系の残りを分析することもできるが, この本の残りではそれらの大部分は冗長である. 幸いなことに, 今やPersistent, フォーム, セッション, サブサイトの章を読んでいるのだからより多くのことがわかるはずである.






