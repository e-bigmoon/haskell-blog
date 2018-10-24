---
title: Yesod 型クラス
date: 2018/09/15
---

あらゆる Yesod アプリケーションは `Yesod` 型クラスのインスタンスでなければなりません。今までのアプリケーションは、`Yesod` 型クラスのデフォルト実装によって動作していました。この章では `Yesod` 型クラスのメソッドをいくつかご紹介します。

`Yesod` 型クラスはアプリケーションの設定を定義するために存在します。全てのアプリケーションには、基本的なデフォルト実装が定義されていますが、より複雑にカスタマイズされたアプリケーションを作るためには、たいていこれらメッソドのいくつかを上書きして使います。

<div class="yesod-book-notice">
"なせレコード型の代わりに型クラスを利用するのだろうか？" という疑問があるかもしれません。型クラスには、2つの主な利点があります。

- Yesod 型クラスのメソッドで別のメソッドを呼ぶことがあります。型クラスの場合は単純ですが、レコード型の場合には多少複雑です。
- もう1つは、構文の単純さです。私たちはデフォルトの実装を提供し、ユーザが必要な機能だけを上書きできるようにしたいのです。型クラスで上書きすることは簡単ですし、構文的にも非常に優れています。また、レコード型には多少のオーバーヘッドがあります。
</div>

## URL のパージングとレンダリング

既に述べた通り、Yesod は型安全URL を通常の URL (HTML に挿入可能なテキスト形式) へ自動的にレンダリングできます。例えば、次のようなルートがあったとしましょう。

```haskell
mkYesod "MyApp" [parseRoutes|
/some/path SomePathR GET
]
```

hamlet テンプレートに `SomePathR` が出現するとき、 Yesod は `SomePathR` をどのようにレンダリングするのでしょうか？Yesod は常に絶対 (***absolute***) URL を構成しようとします。これは XML サイトマップや Atom フィードの生成、電子メールの送信などを考慮するためです。しかし、絶対 URL を作るためには、アプリケーションのドメイン名を知る必要があります。

ドメイン名はユーザのリクエストから得られると思うかもしれませんが、それ以外にもポート番号についても知る必要もあります。そして、例えポート番号をリクエストから取得できたとしても、HTTP または HTTPS のどちらを利用すれば良いのでしょうか？さらにそれを知ったとしても、このような方法はユーザがどのようにリクエストを送信したかに応じて異なる URL が生成されることを意味します。例えば、ユーザが `"example.com"` と `"www.example.com"` にアクセスしたとき、両者で異なる URL が生成されてしまいます。サーチエンジン最適化の観点で考えると、ひとつの正規化された URL に統一したいでしょう。

最後に Yesod はアプリケーションをどこにホストしているかについて何の仮定もありません。例えば、大部分は静的なサイト ([http://static.example.com/](http://static.example.com/)) ですが、 /wiki/ に Yesod 製の Wiki を作りたいとしましょう。このとき、サブパスがどこからホストされているかについて、アプリケーションが決定するための信頼できる方法はありません。なので、様々な予測をするのではなく、Yesod にアプリケーションのルートを教えます。

```haskell
instance Yesod MyWiki where
    approot = ApprootStatic "http://static.example.com/wiki"
```

末尾のスラッシュがないことに注意してください。Yesod が `SomePathR` の URL を構成するときは、まず `SomePathR` が相対パス `/some/path` であるということを計算し、それを `approot` に付け加えます。その結果 `http://static.example.com/wiki/some/path` ができあがります。

`approot` のデフォルト値は `ApprootRelative` です。これは本質的には "何の接頭辞も加えない" ということを意味するため、生成される URL は `/some/path` になります。これは、アプリケーション内リンクの一般的なケースとドメインのルートにホストされたアプリケーションにおいては正しく機能します。しかし、 (電子メールを送る場合のような) 絶対 URL が必要となる場合は `ApprootStatic` を利用するべきでしょう。

`ApprootStatic` コンストラクタに加え `ApprootMaster` と `ApprootRequest` コンストラクタも利用可能です。`ApprootMaster` は `approot` をファウンデーション値から決定します。このコンストラクタを使えば、例えば、`approot` を設定ファイルから読み込むことができます。`ApprootRequest` は `approot` を決めるための追加的なリクエスト値を利用できるため、ユーザが最初にサイトに対してどんなリクエストをしたかによって、異なるドメイン名にすることができます。

scaffolded サイトはデフォルトで `ApprootMaster` を使い、`approot` を `APPROOT` 環境変数、または設定ファイルから起動時に読み込みます。そのため、テストとプロダクションのビルド毎で異なる設定を読み込み、ローカルホストのような1つのドメインをテストし、異なるドメインへデプロイするといった事が簡単にできます。そして、これらの値は設定ファイルから変更できます。

### joinPath

型安全URL をテキスト値に変えるために Yesod は2つの補助関数を使います。1つは `RenderRoot` 型クラスの `renderRoute` メソッドです。全ての型安全URL はこの型クラスのインスタンスです。`renderRoute` は値をパス断片のリストに変換します。例えば先ほどの `SomePathR` は `["some","path"]` に変換されます。

<div class="yesod-book-notice">
実際には `renderRoute` はパス断片のリストと、クエリ文字列パラメータのリストの両方を生成します。`rendeRoute` のデフォルト定義では、クエリ文字列パラメータは常に空リストですが、型クラスなのでメソッドを上書きして使うこともできます。注目すべきケースは静的サブサイトで、ファイルコンテンツのハッシュをキャッシュ目的でクエリ文字列に追加します。
</div>

Yesod 型クラスに定義されている別の関数として `joinPath` メソッドがあります。この関数は以下の4つの引数を取ります。

- ファウンデーション値
- アプリケーションルート
- パス断片のリスト
- クエリ文字列パラメータのリスト

そして、テキスト形式の URL を返します。デフォルトの実装では、パス断片を前方に現れるスラッシュで分離し、パス断片の前にアプリケーションルートを付け加え、パス断片の後ろにクエリ文字列を付け加えるというような、処理を行います。

もし、デフォルトの URL レンダリングで問題なければ特に変更する必要はありません。しかし、末尾のスラッシュを追加するように URL のレンダリングを変更したい時などでは、このメソッドを上書きする必要があります。

### cleanPath

`joinPath` の反対は `cleanPath` です。この関数がディスパッチ処理でどのように使われているか見てみましょう。

1. ユーザからリクエストされたパス情報は、一連のパス断片に分割されます。
1. パス断片を `cleanPath` 関数に渡します。
1. もし `cleanPath` がリダイレクト (`Left` レスポンス) を返せば、301レスポンスがクライアントに送られます。そのため、強制的に正規化した URL を使うことができます。 (例えば、余分なスラッシュを除去するなど)
1. その他の場合 `cleanPath` からのレスポンス (`Right`) を使って、ディスパッチを試し、もしこれが上手くいけばレスポンスを返します。そうでなければ、404を返します。

これらを組み合わせることで、マスターサイトの URL が変化したとしても、サブサイトは URL について完全な制御が可能です。例を使って、どのようにすれば Yesod が常に末尾のスラッシュを含むようになるか確認してみましょう。

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Blaze.ByteString.Builder.Char.Utf8 (fromText)
import           Control.Arrow                      ((***))
import           Data.Monoid                        (mappend)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import           Network.HTTP.Types                 (encodePath)
import           Yesod

data Slash = Slash

mkYesod "Slash" [parseRoutes|
/ RootR GET
/foo FooR GET
|]

instance Yesod Slash where
    joinPath _ ar pieces' qs' =
        fromText ar `mappend` encodePath pieces qs
      where
        qs = map (TE.encodeUtf8 *** go) qs'
        go "" = Nothing
        go x = Just $ TE.encodeUtf8 x
        pieces = pieces' ++ [""]

    -- We want to keep canonical URLs. Therefore, if the URL is missing a
    -- trailing slash, redirect. But the empty set of pieces always stays the
    -- same.
    cleanPath _ [] = Right []
    cleanPath _ s
        | dropWhile (not . T.null) s == [""] = -- the only empty string is the last one
            Right $ init s
        -- Since joinPath will append the missing trailing slash, we simply
        -- remove empty pieces.
        | otherwise = Left $ filter (not . T.null) s

getRootR :: Handler Html
getRootR = defaultLayout
    [whamlet|
        <p>
            <a href=@{RootR}>RootR
        <p>
            <a href=@{FooR}>FooR
    |]

getFooR :: Handler Html
getFooR = getRootR

main :: IO ()
main = warp 3000 Slash
```

まず `joinPath` の実装を見てください。これは1つを除いて Yesod 型クラスのデフォルト実装からほとんど一語一句そのままにコピーされたものです。異なる部分は、末尾に空文字列を余分に追加した点です。パス断片を扱う場合、空文字列はさらにスラッシュを追加するため、余分な空文字列を追加することで、末尾のスラッシュを強制できるのです。

`cleanPath` は少しだけトリッキーです。まず、前のように空パスをチェックし、もし空パスならそのまま受け渡し、リダイレクトが不要なことを示すために `Right` を返します。また、実際には、以下の問題についてチェックする必要があります。

- ダブルスラッシュがあり、それはパスの途中で空文字列として出現する。
- 末尾のスラッシュが欠けており、空文字列では無い文字列が最後の断片として出現する。

これらどちらの状況にも当てはまらないのであれば、最後の断片のみが空なので、最後の断片を除き全てに基づいてディスパッチを行うべきです。また、そうでない場合は正規化された URL にリダイレクトさせたいので、`joinPath` で全ての空断片を取り除き、末尾にスラッシュを追加します。

## defaultLayout

多くのウェブサイトでは、何らかの一般的なテンプレートを全ページに適用させたいということがあります。`defaultLayout` は、こういった用途にピッタリです。独自の関数を定義し代わりにそれを呼び出すことは同じくらい簡単ですが `defaultLayout` を上書きすることで、Yesod で生成された全てのページ (エラーページ、認証ページ) が自動的にこのスタイルとなります。

このメソッドを上書きしたい場合は `Widget` をタイトル、ヘッドタグ、ボディダグに変換するために `widgetToPageContent` を使い、そして Hamlet テンプレートを `Html` 値に変換するために `withUrlRenderer` を使います。さらに `defaultLayout` 内で Lucius テンプレートのようなウィジェットコンポーネントをさらに追加することもできます。詳細は、前章を確認してください。

scaffolded サイトを使っている場合は `templates/default-layout.hamlet` と `templates/default-layout-wrapper.hamlet` を変更します。前者は大部分の `<body>` タグのコンテンツを含み、後者は doctype や `<head>` タグのような残りの HTML を含みます。

### getMessage

まだセッションについては何も説明していませんが、ここで `getMessage` について言及したいと思います。Web 開発の共通パターンとして、ひとつのハンドラでメッセージをセットし、別のハンドラで表示するというものがあります。例えば、ユーザがフォームを `POST` したら、 "フォーム提出は完了しました" のメッセージとともにリダイレクトさせたいといった場合です。これは一般的に [Post/Redirect/Get](https://en.wikipedia.org/wiki/Post/Redirect/Get) として知られています。

これを実現するために Yesod は次の関数のペアを組み込みで用意しています。`setMessage` はメッセージをユーザセッションにセットし `getMessage` はメッセージを回収します (再び出現しないように削除します)。そのため `getMessage` の結果を `defaultLayout` で表示することが推奨されています。以下の例で確認してみましょう。

```haskell
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import Data.Time (getCurrentTime)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent contents
        mmsg <- getMessage
        withUrlRenderer [hamlet|
            $doctype 5

            <html>
                <head>
                    <title>#{title}
                    ^{headTags}
                <body>
                    $maybe msg <- mmsg
                        <div #message>#{msg}
                    ^{bodyTags}
        |]

getHomeR :: Handler Html
getHomeR = do
    now <- liftIO getCurrentTime
    setMessage $ toHtml $ "You previously visited at: " ++ show now
    defaultLayout [whamlet|<p>Try refreshing|]

main :: IO ()
main = warp 3000 App
```

`getMessage` と　`setMessage` については、セッションの章で詳細に触れます。

## カスタムエラーページ

プロフェッショナルな Web サイトの証の1つとして、適切に設計されたエラーページが挙げられます。Yesod はエラーページを表示するために、自動的に `defaultLayout` を利用しますが、もっと別の方法もあります。それが `errorHandler` メソッドを上書きする方法です。

```haskell
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/error ErrorR GET
/not-found NotFoundR GET
|]

instance Yesod App where
    errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
        setTitle "Request page not located"
        toWidget [hamlet|
<h1>Not Found
<p>We apologize for the inconvenience, but the requested page could not be located.
|]
    errorHandler other = defaultErrorHandler other

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            <a href=@{ErrorR}>Internal server error
            <a href=@{NotFoundR}>Not found
    |]

getErrorR :: Handler ()
getErrorR = error "This is an error"

getNotFoundR :: Handler ()
getNotFoundR = notFound

main :: IO ()
main = warp 3000 App haskell
```

ここでは、カスタマイズした404エラーページを指定します。それぞれのエラー型に対して、カスタマイズしたハンドラを書きたくない場合は `defaultErrorHandler` を利用しましょう。型の制約により `fmap toTypeContent` でメソッドで始める必要がありますが、典型的なハンドラ関数を書くことができます (`TypedContent` については、次の章で詳しく説明します)。

実際に、リダイレクトのような特別なレスポンスさえも利用可能です。

```haskell
errorHandler NotFound = redirect HomeR
errorHandler other = defaultErrorHandler other
```

<div class="yesod-book-notice">
上記のように行うことはできますが、実際にはこのような使い方は推奨しません。なぜなら、404は404そのものであるべきだからです。
</div>

## 外部の CSS と Javascript

<div class="yesod-book-notice">
ここで示された機能は scaffolded サイトに自動的に含まれているため、自分自身で実装する必要はありません。
</div>

Yesod 型クラスにおける、最も強力かつ驚くべきメソッドは `addStaticContent` です。ウィジェットは CSS、 Javascript を含む複数のコンポーネントで構成されていることを思い出してください。CSS/JS は実際にはどのようにしてユーザのブラウザに表示されるのでしょうか？デフォルトでは、ページの `<head>` タグの内側の `<style>`、 `<script>` タグにそれぞれ配置されます。

これは単純かもしれませんが、効率的ではありません。ページに何も変更が加えられていなくても、全てのページロードは CSS/JS を初めから読み込む必要があるためです！本当にやりたいことは、このコンテンツを外部ファイルに保存し HTML から参照することです。

ここで `addStaticContent` が登場します。この関数はコンテンツのファイル名の拡張子 (`css` または `js`)、 コンテンツの MIME タイプ (`text/css` または `text/javascript`)、 コンテンツ自身の3つの引数を取り、次の３つのうちどれか1つを返します。

### Nothing

静的ファイルの保存が起こらない場合、このコンテンツを直接 HTML に埋め込みます。これはデフォルトの挙動です。

### Just (Left Text)

このコンテンツは外部ファイルに保存され、そのファイルを参照するためにテキスト形式のリンクを返します。

### Just (Right (Route a, Query))

先ほどと同じですが、何らかのクエリ文字列パラメータとともに、型安全URLを返します。

`Left` の結果は、静的ファイルを CDN やメモリバックサーバのような外部サーバに保存する時によく使われます。`Right` の結果は、より一般的に用いられ、静的サブサイトにおいてよく使われます。この方法は多くのアプリケーションで推奨されるため scaffolded サイトではデフォルトの動作になっています。

<div class="yesod-book-notice">
次のことを不思議に思うかもしれません。もしこれが推奨される方法だとすれば、なぜデフォルトになっていないのでしょうか？問題としては、それは静的サブサイトの存在や、静的ファイルの場所など、普遍的には当てはまらないいくつかの仮定があるからです。
</div>

scaffolded された `addStaticContent` は便利ないくつかの賢い機能を提供します。

- hjsmin パッケージを使って、自動的に Javascript ファイルを最小化します。
- 出力されるファイル名はファイルコンテンツのハッシュによって決まります。これは、古いコンテンツを気にすることなく、遠い将来までキャッシュヘッダをセット可能なことを意味します。
- ファイル名はハッシュに基づいているため、同じ名前のファイルが存在しても、ファイル名を書き直す必要がないことが保証されています。scaffold コードは、自動的にそのようなファイルの存在を確認し、必要でなければ、コストのかかるディスクIOによる書き込みを回避します。

## 静的ファイルの最適化

グーグルは[静的ファイルを異なるドメインから受信せよ](https://developers.google.com/speed/docs/insights/EnableCompression)という重要な最適化を推奨しています。この方法による利点は、メインのドメインにセットしたクッキーが静的ファイルを取り出す際に送信されないので、帯域幅の節約になります。

これを可能にするために `urlParamRenderOverride` メソッドがあります。このメソッドは、通常のURLレンダリング処理に割り込み、何らかのルートに特別な値をセットします。例えば scaffolding はこのメソッドを次のように定義しています。

```haskell
urlParamRenderOverride :: site
                       -> Route site
                       -> [(T.Text, T.Text)] -- ^ クエリ文字列
                       -> Maybe Builder
urlParamRenderOverride y (StaticR s) _ =
  Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
urlParamRenderOverride _ _ _ = Nothing
```

これは、静的ルートが特別な静的ルートから受信されることを意味し、そのルートは異なるドメインになるよう設定可能です。これは、型安全URL の強力さと柔軟性の重要な例でもあり、たった1行のコードで、全てのハンドラを通して、静的ルートのレンダリングを変更することができてしまうのです。

## 認証 / 認可

単純なアプリケーションでは、各ハンドラ関数の中でパーミッションを確認するとういう方法が簡単で便利です。しかし、それはうまくスケーリングしません。結果的には、より宣言的な方法をとりたいと思うようになるでしょう。よくあるシステムでは ACL、特別な設定ファイル、そして多くの意味のわからない呪文を定義するでしょう。Yesod では、それらはただの Haskell です。3つのメソッドが関連します。

### isWriteRequest

現在のリクエストが "読み取り" あるいは "書き込み" 操作であるかどうかを決定します。デフォルトでは、Yesod は RESTful 原則に従い `GET`、`HEAD`、 `OPTIONS`、 `TRACE` リクエストを読み取り専用、その他は書き込み可能とします。

### isAuthorized

ルート (型安全URL) と、リクエストが書き込み可能なリクエストかどうかを示す真偽値を取ります。結果として、`AuthResult` 型の値を返します。この型の値は次の3種類です。

- `Authorized`
- `AuthenticationRequired`
- `Unauthorized`

デフォルトでは、あらゆるリクエストに対して `Authorized` を返します。

### authRoute

`isAuthorized` から `AuthenticationRequired` が返ってきた時は、与えられたルートへリダイレクトします。もし、ルートが与えられていない場合 (デフォルト) は、401 "要認証" メッセージを返します。

これらのメソッドは yesod-auth パッケージとうまく結びつき、それは scaffolded サイトにおいて、 OpenID、 Mozilla Persona、電子メール、ユーザ名、 Twitter のようないくつのもの認証オプションを提供するために利用されます。より具体的な例については、認証の章で確認します。

## 単純な設定たち

Yesod 型クラスの全てのメソッドが複雑というわけではありません。中には、単純な機能のメソッドもあるので、どんなものがあるか見てみましょう。

### maximumContentLength

サービス拒否 (DoS) 攻撃を防ぐために、Yesod はリクエストのボディサイズを制限します。あるルート (例えば、ファイルのアップロードページなど) では、この制限に引っかかる場合があるかもしれませんが、そういった時に関数を使います。

### fileUpload

リクエストのサイズに応じてアップロードされたファイルをどのように扱うか決定します。2つのもっとも一般的な方法は、ファイルをメモリに保存すること、または、一時ファイルに流すことです。デフォルトでは、小さなリクエストはメモリに保持され、大きなものはディスクに保存されます。

### shouldLogIO

与えられた (ソースとレベル情報を含む) ログメッセージが、ログに送られるべきかどうかを決定します。これにより、多くのデバッグ情報をアプリケーションに送ることが可能になりますが、常に利用するのではなく必要な場合にのみ使ってください。

最新の情報については Yesod 型クラスの Haddock API ドキュメントを確認してください。

## まとめ

Yesod 型クラスは、アプリケーションの設定するための多くの上書き可能なメソッドを持ちます。適切なデフォルト実装が全てのメソッドに用意されているので、上書きするかどうかはアプリケーション毎に好きに決めることができます。`defaultLayout` や `getMessage` といった Yesod 型クラスが提供する構築方法を利用することで、エラーページや認証ページのような Yesod で自動生成されるページを含めて、サイト全体を通し一貫した見た目にすることができます。

本章で紹介したメソッドは Yesod 型クラスの一部です。そのため、利用可能なメソッドの全リストについては Haddock ドキュメントを参照してください。

## 本書のコード

- [Example01.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch06/Example01.hs)
- [Example02.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch06/Example02.hs)
- [Example03.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch06/Example03.hs)
