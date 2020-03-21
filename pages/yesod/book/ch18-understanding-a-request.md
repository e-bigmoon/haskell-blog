---
title: リクエストを理解する
published: 2020/02/02
# updated: 2018/04/07
---

Yesod の内部で起きていることを理解せずに Yesod を使い続けることもできます。しかし、内部動作の理解は Yesod を使う上で有益です。この章では、良くある Yesod アプリケーションのリクエストハンドリング処理を扱います。 この章の内容の多くは Yesod 1.2 の変更を含むことに注意してください。大部分の概念は以前のバージョンと変わりませんが、必要とするデータ型は少しごちゃごちゃしているかもしれません。

Yesod はボイラープレートコードを減らすために Template Haskell を利用します。そのため、この部分の処理を理解することがほんの少し難しくなります。もし、本章の範囲を超えてさらに分析したければ、`-ddump-splices` オプションを使って GHC の生成したコードを読むと良いでしょう。

<div class=yesod-book-notice>
本章の情報の多くは Yesod 1.2 におけるブログシリーズとして公開されたものが元になっています。

- [Yesod 1.2’s cleaner internals](https://www.yesodweb.com/blog/2013/03/yesod-1-2-cleaner-internals)
- [Big Subsite Rewrite](https://www.yesodweb.com/blog/2013/03/big-subsite-rewrite)
- [Yesod dispatch, version 1.2](https://www.yesodweb.com/blog/2013/03/yesod-dispatch-version-1-2)
</div>


## ハンドラ

Yesod のリクエストハンドリングを理解する際、2つのコンポーネントを確認する必要があります。具体的には

1. リクエストが適切なハンドラコードにどのようにディスパッチされるか
1. どのようにハンドラ関数が処理されるか

という2つです。

まずはハンドラ関数の処理方法について確認し、その後ディスパッチ処理自身の理解に戻ってくることにします。

### レイヤ

Yesod は自身を WAI の上に構築し、Web サーバのプロトコル (または、より一般的にはハンドラ) と互いに通信するためのアプリケーションを提供します。これは2つのデータ型 `Rewuest` と `Response` で表現され、アプリケーションは以下の型として定義されます。

```haskell
type Application = Request
                -> (Response -> IO ResponseReceived)
                -> IO ResponseReceived
```

WAI ハンドラはアプリケーションを受け取り、それを実行します。

<div class=yesod-book-notice>

`Application` の構造は少し複雑に見えます。それは `bracket` 関数と同じように継続渡し方式を用いてアプリケーションが安全にリソースを取得できるようにしているためです。詳細については WAI API ドキュメントを確認してください。

</div>

Request と Response はどちらも非常に低レベルであり、HTTP プロトコルをそのまま表現しようとしています。そのため WAI は一般的なツールとなっていますが、Web フレームワークを実装するために必要な情報がかなり不足しています。例えば、WAI はすべてのリクエストヘッダの未加工データを提供しますが、Yesod はクッキーの情報を取得するためにデータをパーズし、さらに、セッション情報を取得するためにクッキーをパーズする必要があります。

この問題を扱うために Yesod は2つの新しいデータ型 `YesodRequest` と `YesodResponse` を導入しました。`YesodRequest`は WAI リクエストを含み、それに加えてクッキーやセッション変数のようなリクエスト情報を追加します。レスポンス側では標準的な WAIレスポンスか、更新されたセッション情報や追加のレスポンスヘッダを含むレスポンスの高レベルの表現のどちらかです。WAI アプリケーションを並列化するために次の型を持ちます。

```haskell
type YesodApp = YesodRequest -> ResourceT IO YesodResponse
```

<div class=yesod-book-notice>
Yesod は例外安全のために継続渡し方式ではなく `ResourceT` を利用します。そのため Yesod では例外安全コードがとても簡単に書けます。
</div>

しかし、実際のところ Yesod ユーザが `YesodApp` を見ることは無いでしょう。なぜなら、その上には使い慣れている `HandlerT` レイヤが存在するからです。ハンドラ関数を書くときは3つの異なるものへアクセスする必要があります。

- 現在のリクエストに対する `YesodRequest` の値
- どのようにメッセージをログに残したり、エラー状況を扱うかのような何らかの基本的な環境情報。これは `RunHandlerEnv` によって提供されます。 
- 返されるヘッダやユーザのセッション情報のような、更新可能な情報を追跡し続けるための可変変数。これは `GHState` と呼ばれています。(良い名前ではないことはわかっていますが、歴史的理由でこうなっています)

つまり、ハンドラ関数を書いている時というのは、本質的には単にあらゆるこれらの情報にアクセスできる `ReaderT` 変換子の中でコードを記述していることと同じです。`runHandler` 関数は `HandlerT` を `YesodApp` に変換します。`yesodRunner` はこれを一歩先に進め、しっかりと WAI の `Application` に変換します。

## コンテンツ

これまでに見てきた多くの例では、ハンドラは `Handler Html` の型を持ちます。私たちは `Handler` が何を意味するかについて記述しただけなのに、どうして Yesod は `Html` の扱い方を知っているのでしょうか？答えは `ToTypedContent` 型クラスにあります。関係するコードを示します。


```haskell
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

`Content` データ型はレスポンスボディを与えるための異なる方法を表すための型です。最初の3つは WAI の表現を直接反映します。4番目 (`ContentDontEvaluate`) はレスポンスボディがユーザに返される前に完全に評価されるべきかを表すために用いられます。完全評価における利点は、もし純粋なコードから例外が投げられた際に意味のあるエラーメッセージを与えることができるという点です。欠点としては、おそらく処理時間やメモリー消費が増えることです。

いずれにしても Yesod はどのように `Content` をレスポンスボディに変換するかを知っています。`ToContent` 型クラスは多くの異なるデータ型をレスポンスボディに変換するための方法を与えます。多くの共通に用いられるデータ型はすでに `ToContent` のインスタンスになっています。例えば、正格 `ByteString`、遅延 `ByteString`、正格 `Text`、遅延 `Text`、`Html` などです。

`TypedContent` は追加の情報として、値のコンテンツタイプを持ちます。予想通り、多くの共通のデータ型に対して `ToTypedContent` のインスタンスが定義されています。例えば、`Html`、aeson の `Value` (JSON のため)、`Text` (プレーンなテキストとして扱われる) などです。

```haskell
instance ToTypedContent J.Value where
    toTypedContent v = TypedContent typeJson (toContent v)
instance ToTypedContent Html where
    toTypedContent h = TypedContent typeHtml (toContent h)
instance ToTypedContent T.Text where
    toTypedContent t = TypedContent typePlain (toContent t)
``` 

これらをまとめると、`Handler` は `ToTypedContent` のあらゆるインスタンスを返すことができ、Yesod はそれを適切な表現にし、 Content-Type レスポンスヘッダをセットする。

## 短絡レスポンス

もう一つの変わっている点は短絡についてです。例えば、ハンドラ関数の途中でリダイレクトを呼び出し、関数の残りの部分は呼び出されないというようなことです。このメカニズムには標準的な Haskell の例外が使われています。`ridirect` を呼び出すと `HandlerContents` 型の例外を投げます。`runHandler` 関数は投げられたあらゆる例外をキャッチし、適切なレスポンスを生成します。`HandlerContents` 型の各コンストラクタはリダイレクトやファイル送信など、実行する明確なアクションを与えます。他のすべての例外型では、エラーメッセージがユーザに表示されます。

## ディスパッチ

ディスパッチというのは入ってくるリクエストを受け取り、適切なレスポンスを生成することです。ディスパッチの方法によっていくつかの異なる制約があります。

- パスの断片 (あるいは一片) に基づいたディスパッチ
- リクエストメソッドに基づいた任意ディスパッチ
- サブサイトのサポート。特定の URL プレフィックスの下、複数のルートを与える機能を持つパッケージ化されたコレクション
- WAI アプリケーションをサブサイトとして利用することをサポートする一方で、プロセスに対しできる限り実行時オーバーヘッドをかけないようにする。特に、必要なければ YesodRequest を生成するための不必要なパージンングを行うことを避けたい。

こらを全て満たすためには単に WAI `Application` を利用すれば良いでしょう。しかし、そうすると十分な情報は提供されません。私たちはファウンデーション型、ロガー、サブサイトのルートから親サイトのルートへの変換方法などの情報にアクセスする必要があります。これに対処するために2つの補助的なデータ型 `YesodRunnerEnv` と `YesodSubRunnerEnv` があります。これらは、追加情報を通常のサイトとサブサイトに与えます。

これらの型を利用することでディスパッチは今や比較的単純なものになります。環境とリクエストを与えれば、レスポンスが返ってくるでしょう。これは `YesodDispatch` と `YesodSubDispatch` により表現されます。

```haskell
class Yesod site => YesodDispatch site where
    yesodDispatch :: YesodRunnerEnv site -> W.Application

class YesodSubDispatch sub m where
    yesodSubDispatch :: YesodSubRunnerEnv sub (HandlerSite m) m
                     -> W.Application
``` 

少し後で `YesodSubDispatch` の使い方をお見せします。その前に、`YesodDispatch` がどのように動作するか理解することから始めましょう。

## toWaiApp, toWaiAppPlain, and warp

さて、とりあえず `YesodDispatch` のインスタンスになっているデータ型があるとしましょう。今、何らかの方法で実際にこれを実行したいです。そのためには WAI `Application` に変換し、何らかの WAI ハンドラ/サーバへ受け渡す必要があります。これらを行うために、まずは `toWaiAppPlain` を使います。この関数は必要となるあらゆるアプリケーション毎の初期化を行います。執筆時では、これはロガーを配置し、セッションバックエンドをセットアップすることを意味していますが、将来的に他の機能が追加されるかもしれません。このデータを用いて、`YesodRunnerEnv` を作ることができます。そして、その値が `YesodDispatch` に受け渡されると WAI `Application` になります。

これでほとんど終わりです。最後に残った変更はパス断片のクリーンアップです。`Yesod` 型クラスは正規化された URL を作るための `cleanPath` と呼ばれるメンバ関数を含みます。例えば、デフォルトの実装ではダブルスラッシュは取り除かれ、ユーザを `/foo//bar` から `/foo/bar` にリダイレクトします。`toWaiAppPlain` は必要に応じてリクエストされたパスを分析し、クリーンアップ/リダイレクトを実行することで標準的な WAI リクエストに対して何らかの前処置を追加します。

この時点で完全に機能する WAI `Appplication` を手に入れたことになります。そのほかに2つの補助関数が含まれています。 `toWaiApp` は `toWaiAppPlain` をラップし、さらに一般的に用いられる WAI ミドルウェアを含みます。その中には、リクエストログや GZIP 圧縮が含まれます (最新のリストについては Haddock を参照してください)。最後に `warp` 関数は推察の通り Warp によりアプリケーションを実行します。

<div class=yesod-book-notice>
同様に `PORT` 環境変数からポート番号情報を読む `warpEnv` 関数も存在します。これは、Keter デプロイメントマネージャーや FP Haskell Center などのいくつかのツールと相互作用するために利用されます。
</div>

## 生成されたコード

最後に残ったブラックボックスは Tempkate Haskell で生成されたコードです。この生成されたコードはサイトの退屈でエラーに陥りやすい部分を担当しています。もし望むのであれば、代わりにこれらすべてを手書きすることもできます。翻訳がどのようになるかを実際に確認し、このプロセスにおいてどのように YesodDispatch と YesodSubDispatch が機能するかを解明します。どこにでもあるような Yesod アプリケーション例から始めましょう。

```haskell
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

完全性のために全てのコードを掲載しましたが、ここでは Template Haskell の部分にのみ着目しましょう。

```haskell
mkYesod "App" [parseRoutes|
/only-get       OnlyGetR   GET
/any-method     AnyMethodR
/has-param/#Int HasParamR  GET
/my-subsite     MySubsiteR WaiSubsite getMySubsite
|]
```

これは少しのコードしか生成しませんが、サイトが機能するために3つの要素にのみ再現すれば良いです。もっとも簡単なものは `Handler`型シノニムです。

```haskell
type Handler = HandlerT App IO
```

次は型安全 URL とそのレンダリング関数です。レンダリング関数はパス断片とクエリ文字列パラメータの両方を生成できます。標準的な Yesod サイトはクエリ文字列パラメータを絶対に生成しませんが、技術的には可能です。これは、サブサイトでたまに必要になる。以下の `MySubsiteR` において、qs パラメータの処理方法に注目してください。

```haskell
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

高レベルのルート構文と RenderRoute インスタンスからのかなり単純なマッピングがあるのが分かるでしょう。各ルートは構成子になり、各 URL パラメータはその構成子に対する引数になります。サブサイトに対しルートを埋め込み、パラメータをテキストにレンダリングするために `toPathPiece` を使います。

最後の要素は `YesodDispatch` インスタンスです。これを少しのコードで見てみましょう。

```haskell
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

先ほど説明したように `yesodDispatch` には環境と WAI `Request` の値が渡されます。私たちは現在、リクエストパスあるいは WAI の用語でいうと `pathInfo` に基づきディスパッチを行います。元の高レベルのルート構文を参照し直すと、最初のルートは単一断片の only-get なので今回パターンマッチするものになります。

一度そのマッチが成功すればリクエストメソッドに基づき、さらにパターンマッチを行います。もし、それが `GET` であれば `getOnlyGetR` ハンドラ関数を利用する。それ以外は 405 bad method レスポンスを返したいので、`badMethod` ハンドラ関数を使います。この時点で、オリジナルのハンドラに関する討論を完全に一周しました。ハンドラ関数を実行するために `yesodRunner` 関数を利用していることがわかると思います。`yesodRunner` は環境と WAI `Request` を取り、`RunHandlerEnv` を構築し、それをハンドラ関数に渡した結果の `YesodResponse` を WAI `Response` に変換する関数だったことを思い出してください。

素晴らしい！1つ終わりました。残りはあと3つです。次はさらに簡単です。

```haskell 
            ["any-method"] ->
                yesodRunner handleAnyMethodR env (Just AnyMethodR) req
```

`onlyGetR` と違って `AnyMethodR` はどんなリクエストメソッドに対しても機能するため、これ以上のパターンマッチは必要ありません。

```haskell
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

ここで、動的パラメータという複雑なものを追加します。前の例では `toPathPiece` を使ってテキスト値にレンダリングしましたが、今回は `fromPathPiece` を利用してパージングを行います。パーズが成功すると、次に `OnlyGetR` と非常に類似したディスパッチシステムによって処理される。主要な相違点としては、パラメータをハンドラ関数とルートのデータコンストラクタの両方に渡す必要があるという点です。

次にサブサイトについて見てみましょう。これは、今までとかなり違ったものになります。

```haskell
            ("my-subsite":rest) -> yesodSubDispatch
                YesodSubRunnerEnv
                    { ysreGetSub = getMySubsite
                    , ysreParentRunner = yesodRunner
                    , ysreToParentRoute = MySubsiteR
                    , ysreParentEnv = env
                    }
                req { pathInfo = rest }
```

他のパターンマッチと違って、ここではパターンの接頭辞が一致するかを見るだけです。`/my-subsite` で始まるルートはすべて、処理のためにサブサイトを通過するべきです。これは最終的に `yesodSubDispatch` を用いることになる場所です。この関数は `yesodDispatch` をかなり反映しています。私たちは yesodSubDispatch に渡される新しい環境を構築する必要があります。4つのフィールドについて議論しましょう。

- `ysreGetSub` はサブサイトのファウンデーション型をマスターサイトから取得する方法を明示しています。ここでは `getMySubsite` を与えていますが、これは高レベルのルート構文において提供される関数です。
- `ysreParentRunner` はハンドラ関数を実行するための方法を与えます。単に `yesodRunner` を与えるだけではつまらないように見えるかもしれませんが、別々のパラメータを持つことで深くネストされたサブサイトの構築が可能となるため、相互的なサブサイトにおける多くの層をラップしたり、アンラップしたりできます。(これは、より進んだ概念なのでこの章でカバーしません)
- `ysreToParentRoute` はサブサイトのルートを親サイトのルートに変換します。これは `MySubsiteR` 構成子の目的です。これにより、サブサイトが `getRouteToParent` のような関数を使うことができるようになります。
- `ysreParentEnv` は単に初期環境を渡すだけです。この初期環境には、サブサイトがたぶん必要となる (ロガーのような) 多くのものを含みます。

他の興味深いことは、どのように `pathInfo` を変更するかということです。これにより、サブサイトが親サイトが去った場所からディスパッチを継続 (*continue dispatching*) することが可能になります。これがどのように機能するか確認するために、次の図の様々なリクエストのスクリーンショットを見てください。

<span style="font-size: 1.6rem; display: block;">Path info in subsite</span>
![image/subsite-path-info](https://www.yesodweb.com/book-1.4/image/subsite-path-info)

そして最後に、すべてのリクエストが妥当なルートであるとは限りません。そのような場合は単に 404 not found で対応したいです。


```haskell
            _ -> yesodRunner (notFound >> return ()) env Nothing req
```

## 完全なコード

以下は Template Haskell を使わずに書いた完全なコードです。

```haskell
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

Yesod は開発者から見えないように、かなり多くの配管作業を抽象化して隠しています。この大部分は喜んで無視するようなボイラープレートコードです。しかし、水面下で何か起こっているかということを正確に理解すれば、Yesod を使ってできることがもっと増えることでしょう。現時点で、Haddock の助けを借りることで、たぶんですが、自動生成される Template Haskell コードが無くてもサイトを書くことができると思います。しかし、それは推奨していません。生成コードを用いる方が簡単で安全です。

この章を理解することで1つ特に有益なことは、WAI の世界のどこに Yesod が位置しているかわかるようになることです。これによって、どのように Yesod が WAI ミドルウェアと相互作用しているか、Yesod アプリケーションの他の WAI フレームワークからコードをインクルードする方法を簡単に確認できるようになります (逆もまた然り!)。