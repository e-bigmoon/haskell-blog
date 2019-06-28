---
title: リクエストを理解する
date: 2019/01/20
---

しばしば, かなりの間, その内部構造を理解する必要なしに, Yesodを使い続けてしまうことが起こり得る. しかし, これを理解することは, しばしば有益である. この章では, かなり典型的なYesodアプリケーションに対し, リクエストハンドリングプロセスを扱う. かなり多くのこの議論は, Yesod 1.2におけるコード変更を含むことに注意せよ. 大部分の概念は前のバージョンと同じであるが, 関連するデータ型は少し乱雑かもしれない.

ボイラプレートをバイパスするためにYesodがTemplate Haskellを使用するため, このプロセスを理解するのが時々, 困難になる. もし, この章の情報を超えてさらに分析したければ, -ddump-splicesを用いて, GHCの生成したコードを見ることは役に立つものである. 

<div class=yesod-book-notice>
多くの情報はもともと, ブログシリーズの1.2リリースにおいて公開された. ブログポストは以下で見れる:

- [Yesod 1.2’s cleaner internals](https://www.yesodweb.com/blog/2013/03/yesod-1-2-cleaner-internals)

- [Big Subsite Rewrite](https://www.yesodweb.com/blog/2013/03/big-subsite-rewrite)

- [Yesod dispatch, version 1.2](https://www.yesodweb.com/blog/2013/03/yesod-dispatch-version-1-2)
</div>


## ハンドラ

Yesodのリクエストハンドリングを理解する際, 2つの要素を見る必要がある: リクエストが適切なハンドラコードにどのようにディスパッチされるかと, どのようにハンドラ関数が処理されるかである. 後者から始め, ディスパッチ処理自身の理解に戻ってくることにする. 

### レイヤ

Yesodは自身をWAIの上に構築し, ウェブサーバのプロトコル(または, より一般的にはハンドラ)と, 互いに通信するためのアプリケーション提供する. これは2つのデータ型により表現される: リクエストとレスポンスである. そして, アプリケーションは型として定義される. 

``` haskell
type Application = Request
                -> (Response -> IO ResponseReceived)
                -> IO ResponseReceived
```

WAIハンドラはアプリケーションを取り, それを実行する. 

<div class=yesod-book-notice>

`Application`の構造は, 少し複雑に見える. それは, `bracket`関数と同様に, 継続渡し方式を用いてアプリケーションが安全にリソースを取得できるようにする. より詳細については, WAI API文書を見なさい. 

</div>

リクエストとレスポンスは, 両方ともとても低レベルにあり, HTTPプロトコルをあまり多くの装飾なしに表現しようとしている. これにより, WAIは一般的なツールとなっているが, ウェブフレームワークを実装するための多くの情報を省いている. 例えば, WAIはすべてのリクエストヘッダのための生データを提供する. しかし, Yesodはそれをパーズし, クッキーの情報を取得し, そして, セッション情報を取得するためにクッキーをパーズする必要がある. 

この2分性を扱うために, Yesodは2つの新しいデータ型を導入した: `YesodRequest`と`YesodResponse`である. `YesodRequest`はWAIリクエストを含み, また, クッキーやセッション変数のようなリクエスト情報を追加する.  そして, レスポンス側では, 標準的WAIレスポンスや, 更新されたセッション情報や追加のレスポンスヘッダを含むレスポンスの, 高レベルの表現であったりする. WAIアプリケーションを並列化するために, 次を持っている.

``` haskell
type YesodApp = YesodRequest -> ResourceT IO YesodResponse
```

<div class=yesod-book-notice>
Yesodは継続渡し方式を用いる代わりに, 例外安全のために`ResourceT`を用いる. これにより, Yesodにおいて例外安全コードを用いることがずっと容易になる. 
</div>

しかし, Yesodユーザとして, 実際には`YesodApp`は決して見ないであろう. その上には使い慣れているであろう他のレイヤが存在する: それは, `HandlerT`である. ハンドラ関数を書くとき, 3つの異なるものへアクセスする必要がある:

- 現在のリクエストに対する`YesodRequest`値

- どのようにメッセージをログに残したり, エラー状況を扱うかのような何らかの基本的な環境情報. これは, `RunHandlerEnv`により提供される. 

- 返されるヘッダや, ユーザのセッションデータのような, 更新される情報を追跡するための可変変数. これは, `GHState`と呼ばれている. (これは良い名前でないことは知っているが, 歴史的理由によりそうなっている.)

したがって, ハンドラ関数を書くにおいて, 本質的には単にあらゆるこれらsの情報にアクセスを持つ`ReaderT`変換子の中で書いているのに過ぎない. `runHandler`関数は`HandlerT`を`YesodApp`に変換する. `yesodRunner`はこれを一歩先に進め, しっかりとWAIアプリケーションに変換する.

## コンテンツ

上の例と, すでに見たであろう多くの例では, ハンドラに`Handler Html`の型を与える. ちょうど`Handler`が何を意味するかについて示したが, どうしてYesodは`Html`の扱い方を知っているのであろうか? 答えは`ToTypedContent`型クラスに存在する. 関連するコードを示す:   


``` haskell

data Content = ContentBuilder !BBuilder.Builder !(Maybe Int) -- ^ The content and optional content length.
             | ContentSource !(Source (ResourceT IO) (Flush BBuilder.Builder))
             | ContentFile !FilePath !(Maybe FilePart)
             | ContentDontEvaluate !Content
data TypedContent = TypedContent !ContentType !Content

class ToContent a where
    toContent :: a -> Content
class ToContent a => ToTypedContent a where
    toTypedContent :: a -> TypedContent

```

`Content`データ型はレスポンスボディを与えるための別の方法を表す. 最初の3つは, WAI表現を直接反映する. 4番目(`ContentDontEvaluate`)は, レスポンスボディがユーザに返される前に完全に評価されるべきかを示すために用いられる. 完全評価における利点は, もし純粋なコードから例外が投げられた際, 意味のあるエラーメッセージを与えることができることである. 欠点としては, おそらく時間やメモリー消費が増えることである. 

いずれにしても, Yesodはどのように`Content`をレスポンスボディに変換するかを知っている. `ToContent`型クラスは多くの異なるデータ型をレスポンスボディに変換するための方法を与える. 多くの共通に用いられるデータ型はすでに`ToContent`のインスタンスであり, strictやlazy `ByteString`, そして`Text`, もちろん`Html`も含む. 

`TypedContent`は追加の情報を加える: それは値のコンテンツタイプである. 予想通り, 多くの共通のデータ型に対し, `ToTypedContent`インスタンスがあり, `Html`, (JSONにおける)aeson`Value`, そして`Text`(プレーンなテキストとして扱われる)が含まれる.

``` haskell

instance ToTypedContent J.Value where
    toTypedContent v = TypedContent typeJson (toContent v)
instance ToTypedContent Html where
    toTypedContent h = TypedContent typeHtml (toContent h)
instance ToTypedContent T.Text where
    toTypedContent t = TypedContent typePlain (toContent t)

``` 

これらをすべてまとめる: `Handler`は`ToTypedContent`のあらゆるインスタンスを返すことができ, Yesodはそれを適切な表現にし, コンテンツタイプレスポンスヘッダをセットすることで対処する. 

## 短絡レスポンス

もう一つの変わった点としては, どのように短絡が機能するかである. 例えば, ハンドラ関数の途中でリダイレクトを呼び出すことができ, 残りの関数は呼び出されない. 使われているメカニズムは標準Haskell例外である. `ridirect`を呼び出すことで, `HandlerContents`型の例外を投げる. `runHandler`関数は投げられたあらゆる例外をキャッチし, 適切なレスポンスを生成する. `HandlerContents`に対し, それがリダイレクション, ファイル送信であれ, 各コンストラクタは実行すべき明確なアクションを与える. 他のすべての例外型については, エラーメッセージがユーザに示される. 

## ディスパッチ

ディスパッチは入ってくるリクエストを受け取り, 適切なレスポンスを生成することである. どのようにディスパッチを扱うかについて, いくつかの異なる制限がある:

- パスの断片(あるいは一片)に基づいたディスパッチ.

- リクエストメソッドに基づいた任意ディスパッチ.

- サブサイトのサポート : 特定のURLプレフィックスの下, 多数のルートを与える関数のパッケージ化された集合.

- WAIアプリケーションをサブサイトとして用いることをサポートする一方, プロセスに対しできる限り実行時オーバーヘッドをかけないようにする. 特に, 用いることがない場合に, YesodRequestを生成するための不必要なパージンングを行うことを避けたい. 

これを行うための最小公倍数は, 単にWAI`Application`を用いることである. しかし, これは十分な情報は与えてくれない: ファウンデーションデータ型. ロガー, そして, サブサイトに対し, どのようにサブサイトのルートが親サイトのルートに変換されるかについてのアクセスが必要になる. これに対処するために, 2つの補助データ型がある. それは, `YesodRunnerEnv`と, `YesodSubRunnerEnv`であり, この追加情報を通常のサイトとサブサイトに与える. 

これらの型を用いることで, ディスパッチは今や比較的単純なものになる: 環境とリクエストが与えられれば, レスポンスが返ってくる. これは, `YesodDispatch`と, `YesodSubDispatch`により表現される:

``` haskell
class Yesod site => YesodDispatch site where
    yesodDispatch :: YesodRunnerEnv site -> W.Application

class YesodSubDispatch sub m where
    yesodSubDispatch :: YesodSubRunnerEnv sub (HandlerSite m) m
                     -> W.Application
``` 

少し後にどのように`YesodSubDispatch`が用いられているかについて示す. まずはじめに, どのように`YesodDispatch`が登場したかについて理解する. 

## toWaiApp, toWaiAppPlain, and warp

今の所, `YesodDispatch`のインスタンスであるデータ型があるとしよう. 今, 何らかの方法で実際にこれを実行したいとする. これを行うために, それをWAI `Application`に変換し, ある種のWAIハンドラ/サーバへ受け渡す必要がある. これを開始するために, `toWaiAppPlain`を使う. これは必要なあらゆるアプリケーション毎の初期化を行う. 執筆の際には, これはロガーを配置し. セッションバックエンドをセットアップすることを意味したが, 将来, 他の機能が追加されるかもしれない. このデータを用いて, `YesodRunnerEnv`を作ることができる. そして, その値が`YesodDispatch`に受け渡されると, WAI `Application`を得ることができる. 

これでほとんど終わりである. 最後に残った変更はパス断片のクリーンアップある. `Yesod`型クラスは`cleanPath`と呼ばれるメンバ関数を含み, 正規化されたURLを作るために用いられる. 例えば, デフォルトの実装においては, ダブルスラッシュは除かれ, ユーザを`/foo//bar`から, `/foo/bar`にリダイレクトする. `toWaiAppPlain`は, 必要に応じて, リクエストされたパスを分析し, クリーンアップ/リダイレクトを行うことで, 標準WAIリクエストに対し前処置を追加する. 

この点において, 完全に機能的なWAI `Appplication`を持ったことになる. そのほかに2つのヘルパ関数が含まれている. `toWaiApp`は`toWaiAppPlain`をラップし, さらに, 一般に用いられるWAIミドルウェアを含む. その中には, リクエストログや, GZIP圧縮が含まれる. (最新のリストについては, Haddockを参照してください.) 最後に`warp`関数があり, 推察の通り, Warpによりアプリケーションを実行する. 

<div class=yesod-book-notice>
`warpEnv`関数もまた存在し, `PORT`環境変数から, ポート番号情報を読む. これは, あるツールと相互作用するために用いられ, KeterデプロイメントマネージャやFP Haskell Centerなどがそのツールに含まれる. 
</div>

## 生成されたコード

最後に残ったブラックボックスはTempkate Haskellで生成されたコードである. この生成コードは,  サイトにおいて, 退屈でエラーに陥りやすい部分を扱う責任がある. もし望むのなら, 代わりにこれらすべてを手書きにできる. 翻訳がどのようになるかを実証し, このプロセスにおいてどのようにYesodDispatchとYesodSubDIspatchが機能するかを解明する. かなり典型的なYesodアプリケーションから始めよう. 

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Types         (status200)
import           Network.Wai                (pathInfo, rawPathInfo,
                                             requestMethod, responseLBS)
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/only-get       OnlyGetR   GET
/any-method     AnyMethodR
/has-param/#Int HasParamR  GET
/my-subsite     MySubsiteR WaiSubsite getMySubsite
|]

instance Yesod App

getOnlyGetR :: Handler Html
getOnlyGetR = defaultLayout
    [whamlet|
        <p>Accessed via GET method
        <form method=post action=@{AnyMethodR}>
            <button>POST to /any-method
    |]

handleAnyMethodR :: Handler Html
handleAnyMethodR = do
    req <- waiRequest
    defaultLayout
        [whamlet|
            <p>In any-method, method == #{show $ requestMethod req}
        |]

getHasParamR :: Int -> Handler String
getHasParamR i = return $ show i

getMySubsite :: App -> WaiSubsite
getMySubsite _ =
    WaiSubsite app
  where
    app req sendResponse = sendResponse $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        $ L8.pack $ concat
            [ "pathInfo == "
            , show $ pathInfo req
            , ", rawPathInfo == "
            , show $ rawPathInfo req
            ]

main :: IO ()
main = warp 3000 App
```

完成性のため, 完全なリストを与えるが, Template Haskell部分にのみ着目しよう:

``` haskell
mkYesod "App" [parseRoutes|
/only-get       OnlyGetR   GET
/any-method     AnyMethodR
/has-param/#Int HasParamR  GET
/my-subsite     MySubsiteR WaiSubsite getMySubsite
|]
```

これは少しのコードしか生成しないが, サイトが機能するために3つの要素のみ再現すればよい. もっとも簡単なものから始めよう: `Handler`型シノニムである.

``` haskell
type Handler = HandlerT App IO
```

次は, 型安全URLとそのレンダリング関数である. レンダリング関数はパス断片とクエリ文字列パラメータのどちらも生成することが許可されている. 標準Yesodサイトは決して, クエリ文字列パラメータを生成しないが, それは技術的には可能である. サブサイトにおいては, これはしばしば生じる. `MySubsiteR`において, どのようにクエリ文字列パラメータを処理しているかについて注意せよ. 

``` haskell
instance RenderRoute App where
    data Route App = OnlyGetR
                   | AnyMethodR
                   | HasParamR Int
                   | MySubsiteR (Route WaiSubsite)
        deriving (Show, Read, Eq)

    renderRoute OnlyGetR = (["only-get"], [])
    renderRoute AnyMethodR = (["any-method"], [])
    renderRoute (HasParamR i) = (["has-param", toPathPiece i], [])
    renderRoute (MySubsiteR subRoute) =
        let (ps, qs) = renderRoute subRoute
         in ("my-subsite" : ps, qs)
```

高レベルのルート構文とRenderRouteインスタンスからの, かなり単純なマッピングがあるのが分かるだろう. 各ルートは構成子になり, 各URLパラメータはその構成子に対する引数になる. サブサイトに対しルートを埋め込み, パラメータをテキストにレンダリングするために, `toPathPiece`を用いる. 

最後の要素は, `YesodDispatch`インスタンスである. これを少しのコードで見てみよう. 

``` haskell
instance YesodDispatch App where
    yesodDispatch env req =
        case pathInfo req of
            ["only-get"] ->
                case requestMethod req of
                    "GET" -> yesodRunner
                        getOnlyGetR
                        env
                        (Just OnlyGetR)
                        req
                    _ -> yesodRunner
                        (badMethod >> return ())
                        env
                        (Just OnlyGetR)
                        req
```

上で示したように, `yesodDispatch`は環境とWAI `Request`を渡される. リクエストパス, あるいはWAIの用語でいうと`pathInfo`に基づきディスパッチを行う. 元の高レベルのルート構文を参照し直すと, 最初のルートは単一のonly-getになり, 今回パターンマッチするものになる. 

一度そのマッチが成功すれば, リクエストメソッドに基づき, さらにパターンマッチを行う: もし, それが`GET`の場合, ハンドラ関数である`getOnlyGetR`を用いる. そうでなければ, 405 bad methodレスポンスを返し, したがって, `badMethod`ハンドラ関数を用いる. この点において,  オリジナルのハンドラに関する討論を完全に一周した. ハンドラ関数を実行するために, `yesodRunner`関数を用いているのが分かるであろう. リマインダーとして, これは環境とWAI `Request`を取り, `RunHandlerEnv`を構築し, それをハンドラ関数に渡し, 結果生じる`YesodResponse`をWAIレスポンスに変換する. 

素晴らしい: 1つで, 3つのことができる. 次はさらに容易である.

``` haskell 
    ["any-method"] ->
                yesodRunner handleAnyMethodR env (Just AnyMethodR) req
```

`onlyGetR`と違い, `AnyMethodR`はどのリクエストメソッドに対しても機能するため, さらにパターンマッチをする必要がなくなる. 

``` haskell
["has-param", t] | Just i <- fromPathPiece t ->
                case requestMethod req of
                    "GET" -> yesodRunner
                        (getHasParamR i)
                        env
                        (Just $ HasParamR i)
                        req
                    _ -> yesodRunner
                        (badMethod >> return ())
                        env
                        (Just $ HasParamR i)
                        req
```

ここで1つの複雑なものを追加する: 動的パラメータ. 上では`toPathPiece`を用いてテキスト値にレンダリングしたが, 今回は`fromPathPiece`を用いてパーシングを行う. パースが成功したとして, 次に`OnlyGetR`で用いられたのと, とても類似したディスパッチシステムを用いる. 主要な相違点としては, パラメータがハンドラ関数と, ルートのデータコンストラクタに渡される必要があることである. 

次に, サブサイトについて見るが, かなり違ったものとなる. 

``` haskell
("my-subsite":rest) -> yesodSubDispatch
                YesodSubRunnerEnv
                    { ysreGetSub = getMySubsite
                    , ysreParentRunner = yesodRunner
                    , ysreToParentRoute = MySubsiteR
                    , ysreParentEnv = env
                    }
                req { pathInfo = rest }
```

他のパターンマッチと違って, ここではパターンの接頭辞が一致するかを見るだけである. `/my-subsite`で始まるルートはすべて, 処理のためにサブサイトを通過するべきである. これは最終的に`yesodSubDispatch`を用いることになる場所である. この関数は, `yesodDispatch`をかなり反映している. それに渡される新しい環境を構築する必要がある. 4つのフィールドについて議論しよう.

- `ysreGetSub`はどのように, サブサイトのファウンデーション型をマスターサイトから取得するかを明示している. `getMySubsite`を与えるが, これは, 高レベルのルート構文において与えた関数である. 

- `ysreParentRunner`はハンドラ関数を実行するための方法を与える. 単に`yesodRunner`を与えるだけではつまらないように見えるかもしれないが, 別々のパラメータを持つことで, 深くネストされたサブサイトの構築が可能となり, 相互的なサブサイトにおける多くの層をラップしたり, アンラップしたりできる. (これはこの章においてはカバーしないより進んだ概念である.)

- `ysreToParentRoute`はサブサイトに対するルートを, 親サイトに対するルートに変換する. これは`MySubsiteR`構成子の目的である. これにより, サブサイトが`getRouteToParent`のような関数を使うことが可能になる. 

- `ysreParentEnv`は単に最初の環境を渡すだけであり, (ロガーのような)サブサイトが必要とするかもしれない多くのものを含む. 

他の興味深いことは, どのように`pathInfo`を変更するかである. これにより, サブサイトが親サイトが去った場所からディスパッチを継続することが可能になる. これがどのように機能するかを明示するために, 次の図において様々なリクエストのスクリーンショットを見なさい. 

そして最後に, すべてのリクエストが妥当なルートであるとは限らない. そのような場合, 単に404 not foundで対応したい. 

### Path info in subsite
![image/subsite-path-info](https://www.yesodweb.com/book-1.4/image/subsite-path-info)

``` haskell
_ -> yesodRunner (notFound >> return ()) env Nothing req
```

## 完全なコード

次はTemplate Haskellによらない方法による完全なコードである. 

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Types         (status200)
import           Network.Wai                (pathInfo, rawPathInfo,
                                             requestMethod, responseLBS)
import           Yesod
import           Yesod.Core.Types           (YesodSubRunnerEnv (..))

data App = App

instance RenderRoute App where
    data Route App = OnlyGetR
                   | AnyMethodR
                   | HasParamR Int
                   | MySubsiteR (Route WaiSubsite)
        deriving (Show, Read, Eq)

    renderRoute OnlyGetR = (["only-get"], [])
    renderRoute AnyMethodR = (["any-method"], [])
    renderRoute (HasParamR i) = (["has-param", toPathPiece i], [])
    renderRoute (MySubsiteR subRoute) =
        let (ps, qs) = renderRoute subRoute
         in ("my-subsite" : ps, qs)

type Handler = HandlerT App IO

instance Yesod App

instance YesodDispatch App where
    yesodDispatch env req =
        case pathInfo req of
            ["only-get"] ->
                case requestMethod req of
                    "GET" -> yesodRunner
                        getOnlyGetR
                        env
                        (Just OnlyGetR)
                        req
                    _ -> yesodRunner
                        (badMethod >> return ())
                        env
                        (Just OnlyGetR)
                        req
            ["any-method"] ->
                yesodRunner handleAnyMethodR env (Just AnyMethodR) req
            ["has-param", t] | Just i <- fromPathPiece t ->
                case requestMethod req of
                    "GET" -> yesodRunner
                        (getHasParamR i)
                        env
                        (Just $ HasParamR i)
                        req
                    _ -> yesodRunner
                        (badMethod >> return ())
                        env
                        (Just $ HasParamR i)
                        req
            ("my-subsite":rest) -> yesodSubDispatch
                YesodSubRunnerEnv
                    { ysreGetSub = getMySubsite
                    , ysreParentRunner = yesodRunner
                    , ysreToParentRoute = MySubsiteR
                    , ysreParentEnv = env
                    }
                req { pathInfo = rest }
            _ -> yesodRunner (notFound >> return ()) env Nothing req

getOnlyGetR :: Handler Html
getOnlyGetR = defaultLayout
    [whamlet|
        <p>Accessed via GET method
        <form method=post action=@{AnyMethodR}>
            <button>POST to /any-method
    |]

handleAnyMethodR :: Handler Html
handleAnyMethodR = do
    req <- waiRequest
    defaultLayout
        [whamlet|
            <p>In any-method, method == #{show $ requestMethod req}
        |]

getHasParamR :: Int -> Handler String
getHasParamR i = return $ show i

getMySubsite :: App -> WaiSubsite
getMySubsite _ =
    WaiSubsite app
  where
    app req sendResponse = sendResponse $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        $ L8.pack $ concat
            [ "pathInfo == "
            , show $ pathInfo req
            , ", rawPathInfo == "
            , show $ rawPathInfo req
            ]

main :: IO ()
main = warp 3000 App
```

## 結論

Yesodはかなり多くの配管作業を抽象化し, 開発者から免除している. この大部分は喜んで無視するであろうボイラプレートコードである. しかし, 表面化において何が起こっているかを理かいすることは, 自身につながる. この点において, 幸いなことに, Haddockの助けを借りて, 自動生成されるTemplate Haskellコードなしでサイトを書くことができる. それを推奨しているわけではない; 生成コードを用いる方が容易で安全である. 

この章を理解することで, 1つ特に有益なことは, WAIの世界におけるどこにYesodが位置しているかを見れることである. これにより, どのようにYesodがWAIミドウェアと相互作用したり, Yesodアプリケーションにおける他のWAIフレームワークからコードをインクルードしたりするかを見るのが容易になる(逆もまた然り!).