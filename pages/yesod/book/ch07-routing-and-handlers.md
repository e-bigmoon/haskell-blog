---
title: ルーティングとハンドラ
published: 2018/03/18
updated: 2019/03/03
---

Yesod をモデル・ビュー・コントローラフレームワークとして見れば、ルーティングとハンドラはコントローラに対応します。また、他の Web 開発環境で採用されている2つの異なるルーティング方法があります。

- ファイル名でディスパッチする方法。例えば PHP や ASP などで利用されています。
- 正規表現を使ってルートをパースする中央集権的なルーティング関数を用意する方法。Django や Rails はこの方法を採用しています。

Yesod の仕組み的には Django や Rails と近いのですが、実際にはかなり違います。Yesod は正規表現の代わりに、ルートの断片のマッチを使います。また、単方向のルートとハンドラのマッピングを使うのではなく、Yesod は (ルートデータ型や型安全URLと呼ばれる) 中間データ型と双方向の変換関数を生成します。

このような、発展的なシステムのコーディングは人間が行うと非常に退屈な作業であり、エラーを含みやすいものです。そのため Yesod はルートに特化したドメイン特化言語 (DSL) を定義し、この DSL を Haskell コードに変換するためのテンプレート Haskell 関数を提供します。この章ではルーティング宣言に関する構文を説明し、コード生成がどのように行われるかを確認します。そして、ルーティングとハンドラ間の相互作用について説明します。

## ルート構文

Yesod のアプローチはルート宣言を既存の構文に無理やり組み込むのではなく、ルートのために設計されたシンプルな構文を利用することです。この手法の良い点は、コードが書きやすくなるだけではなく、Yesod の経験が全く無い場合でも、アプリケーションのサイトマップのように、十分に理解しやすいという点です。

基本的な構文は以下のようになります。

```haskell
/             HomeR     GET
/blog         BlogR     GET POST
/blog/#BlogId BlogPostR GET POST

/static       StaticR   Static getStatic
```

このルート宣言で行われる処理の完全な詳細は以降の節で説明します。

### 断片

Yesod がリクエストを受け取った時に最初に行うことの1つは、リクエストされたパスを断片に分割することです。この断片は全てのフォワードスラッシュによってトークン化されます。例えば以下のようにです。

```haskell
toPieces "/" = []
toPieces "/foo/bar/baz/" = ["foo", "bar", "baz", ""]
```

URLの最後に付けるトレイリングスラッシュや2重スラッシュ ("/foo//bar//") のような問題のある状況が起こり得ることに気づくでしょう。Yesod は正規化された URL を持つことを推奨します。もしユーザがトレイリングスラッシュあるいは2重スラッシュ付きでURLをリクエストすれば、それらは自動的に正規化された URL にリダイレクトされます。その結果、1つのリソースにつき1つの URL ということが保証されるため、検索順位に良い影響を与えます。

これが意味することは、URL の正確な構造に悩む必要がないということです。パス断片を安全なものとして考えることができ、Yesod は自動的にスラッシュの挿入を制御し、問題のある文字をエスケープしてくれます。

ところで、どのようにパスが断片に分割され再結合されるかについての詳細な制御方法を知りたい場合は Yesod 型クラスの章の `cleanPath` や `joinPath` メソッドを再確認してみると良いでしょう。

### 断片の種類

ルートを宣言する時は断片の表現方法を次の3つの種類から好きなものを選びます。

#### スタティック

これはただの文字列表現です。URL と正確に一致する必要があります。

#### ダイナミックシングル

これは (2つのスラッシュ間の) 単一の断片ですが、ユーザが送信した値を表します。ページリクエストで追加のユーザ入力を受け取るためによく使います。ダイナミックシングルはハッシュ (#) で始まり、そのあとにデータ型が続きます。また、このデータ型は `PathPiece` 型クラスのインスタンスでなければなりません。

#### ダイナミックマルチ

ダイナミックシングルとほとんど同じですが、URL 断片を複数受け取ることができます。また、この断片は必ずリソースパターンの最後に出現しなければなりません。ダイナミックマルチはアスタリスク (\*) から始まり、その後にデータ型が続きます。また、このデータ型は `PathMultiPiece` のインスタンスでなければなりません。この方法は他の2つと比べて少し変わっていますが、ファイル構造や任意の階層を持つ wiki などの静的ツリー表現のような機能を実装するためにとても重要です。

<div class=yesod-book-notice>
Yesod 1.4 からダイナミックマルチを指定する方法に `+` が追加されました。この方法は C プロセッサが `/*` 文字の組み合わせによっておかしくなってしまう場合などで有効です。
</div>

まずは、標準的なリソースパターンを見てみましょう。一番簡単なものは、アプリケーションルートを `/` にするものです。同じように FAQ を `/page/faq` としたりする場合もあるでしょう。

例えばフィボナッチウェブサイトを作りたいと思ったら URL を `/fib/#Int` のようにするでしょう。しかし、これにはちょっとした問題があります。マイナスの値やゼロをアプリケーションに通したくはありません。幸運なことに型システムはこれをしっかりと防いでくれます。

```haskell
newtype Natural = Natural Int
    deriving (Eq, Show, Read)

instance PathPiece Natural where
    toPathPiece (Natural i) = T.pack $ show i
    fromPathPiece s =
        case reads $ T.unpack s of
            (i, ""):_
                | i < 1 -> Nothing
                | otherwise -> Just $ Natural i
            [] -> Nothing
```

このコードを説明すると、1行目は不適切な入力を避けるために、Int に対して newtype を使って Natural 型を定義します。また、`PathPiece` は2つのメソッドをもつ型クラスだということがわかります。`toPathPiece` は `Text` に変換する以上のことは何も行いません。`fromPathPiece` は `Text` からデータ型への変換を行いますが、この変換に失敗した場合は `Nothing` を返します。この `Natural` 型を使えば、ハンドラ関数に自然数のみが与えられることを保証するため、型システムを使って境界問題に対処できます。

<div class=yesod-book-notice>
現実世界のアプリケーションにおいては、アプリケーション内で有効ではない `Natural` 値を絶対に作れないことを保証したいでしょう。そのようにするためには [smart constructors](https://wiki.haskell.org/Smart_constructors) のような方法があります。ただ、今回の例の目的とは少し違うので、ここでは説明しません。
</div>

`PathMultiPiece` も同じくらい簡単に定義できます。例えば、少なくとも2つ以上の階層をもつ Wiki を作りたいので、次のようなデータ型を定義しましょう。

```haskell
data Page = Page Text Text [Text] -- 2 or more
    deriving (Eq, Show, Read)

instance PathMultiPiece Page where
    toPathMultiPiece (Page x y z) = x : y : z
    fromPathMultiPiece (x:y:z) = Just $ Page x y z
    fromPathMultiPiece _ = Nothing
```

### オーバーラップチェック

Yesod はデフォルトで、どの2つのルートも互いに重なり合う可能性がないことを保証しています。そこで例えば次のようなルートを考えてみましょう。

```haskell
/foo/bar   Foo1R GET
/foo/#Text Foo2R GET
```

このルート宣言は `/foo/bar` が両方のルートに一致してしまうため、オーバーラップしているものとして拒否されます。しかし、オーバーラップを許可したい時があります。

1. データ型の定義からオーバーラップが絶対発生しないとわかっている場合。例えば、上の `Text` を `Int` で置き換えたとすると、オーバーラップするルートが存在しないことはすぐにわかります。しかし、Yesod は今のところそのような分析はできません。
1. アプリケーションがどのように作動するかについて特別な情報を持っているため、そのような状況が絶対に発生しないとわかっている場合。例えば、`Foo2R` ルートの引数で `bar` を受け取ることが絶対に無いといった感じです。

オーバーラップチェックをオフにするためには、エクスクラメーションマークをルートの始まりに追加します。例えば、次のコードは問題なく Yesod で動作します。

```haskell
/foo/bar    Foo1R GET
!/foo/#Int  Foo2R GET
!/foo/#Text Foo3R GET
```

<div class=yesod-book-notice>
エクスクラメーションマークを置く場所は `#`、`*`、`+` 文字の後でも良いですし、どのパス断片の始めに置いても大丈夫です。ただ、新しい構文のほうが目的が明確なためより好まれます。
</div>

オーバーラッピングルートを利用するとルートが曖昧になってしまうという問題が発生します。上記の例において `/foo/bar` は `Foo1R` と `Foo3R` のどちらにルーティングするべきでしょうか？また `/foo/42` は `Foo2R` と `Foo3R` のどちらにルーティングするべきでしょうか？Yesod では、最初のルートが優先されるという単純なルールになっています。

### リソース名

それぞれのリソースパターンは自身と結びつけるための名前を持っています。その名前はアプリケーションの型安全URLデータ型のコンストラクタとなるため、大文字で始まらなければなりません。慣習として、これらのリソース名はすべて大文字の R で終わります。このルールを強制するものは何もなく、わかりやすいのでみんな使っているだけです。

コンストラクタの正確な定義はそれが結びついているリソースパターンに依存しています。パターンの断片がダイナミックシングルであっても、ダイナミックマルチであっても、どちらもパターンで指定された型がコンストラクタの引数の型になります。これはアプリケーションにおいて型安全URLの値と有効な URL の間に1対1の対応があるということを意味します。

<div class=yesod-book-notice>
これは、すべての値に対してページが存在するという意味するのではなく、潜在的には有効な URL だという意味です。例えば `PersonR "Michael"` という値はデータベースに Michael がなければ有効なページにはたどり着かないでしょう。
</div>

より実用的な例として、`PersonR` という名前の `/person/#Text`、`YearR` という名前の `/year/#Int`、`FaqR` という名前の `/page/faq` というリソースパターンがあるとすれば、ルートデータ型はおおよそ以下のようになるでしょう。

```haskell
data MyRoute = PersonR Text
             | YearR Int
             | FaqR
```

もし、ユーザが `/year/2009` をリクエストすれば Yesod はそれを `YearR 2009` という値に変換します。同様に `/person/Michael` は `PersonR "Michael"` となり、 `/page/faq` は `FaqR` なります。一方で `/year/two-thousand-nine`、`/person/michael/snoyman`、`/page/FAQ` は変換されず、404エラーになります。

### ハンドラの仕様

最後に、リソースを宣言する際にそれらがどのように処理されるかということについて説明します。Yesod には次の3つの選択肢があります。

- ルートに対して、すべてのリクエストメソッドを処理するシングルハンドラ関数
- ルートに対して、それぞれのリクエストメソッドを処理するセパレートハンドラ関数。その他のリクエストメソッドの場合は 405 Method Not Allowed を生成します。
- サブサイトに受け流す

はじめの2つは簡単に使えます。シングルハンドラ関数は `/page/faq FaqR` のようなリソースパターンとリソース名だけが1行に書かれています。この場合、ハンドラ関数は `handleFaqR` と命名しなければなりません。

それぞれのリクエストメソッドに対応するためのセパレートハンドラも同じ書き方ですが、リクエストメソッドのリストが必要になります。このリクエストメソッドは必ず全ての文字が大文字でなければなりません。例えば `/person/#String PersonR GET POST DELETE` というようにです。この場合、`getPersonR`、`postPersonR` `deletePersonR` という3つのハンドラ関数を定義する必要があります。

サブサイトはとても便利な機能ですが、 Yesod ではかなり複雑な話題です。サブサイトの書き方については後にしますが、それらを使うことはそれほど難しくありません。最も一般的なサブサイトは静的サブサイトで、アプリケーションのために静的ファイルを配信します。`/static` から静的ファイルを受け取るためには次のようなリソース行が必要となります。

```haskell
/static StaticR Static getStatic
```

この行では `/static` は単にURL構造のどこから静的ファイルを受け取るかということを言っています。static という言葉自体には何も魔法の力はなく `/my/nondynamic/files` などに簡単に変更できます。

次の `StaticR` という単語はリソース名を表します。さらに、その次の2つの単語はサブサイトを使うということを表します。`Static` はサブサイトのファウンデーションデータ型で、`getStatic` はマスターファウンデーションデータ型の値から `Static` 値を取り出すための関数です。

今はサブサイトの詳細にはあまり深入りしないようにしましょう。scaffolded サイトの章で、静的サブサイトについての詳細を確認します。

## ディスパッチ

一度ルートを指定すれば Yesod は URL ディスパッチに関する厄介な詳細をすべて引き受けてくれます。そのためには、確実に適切なハンドラ関数を与えていさえすれば良いのです。サブサイトのルートに関してはハンドラ関数を書く必要はありませんが、他の2つに関しては書く必要があります。命名規則については既に説明しました (`MyHandlerR GET` は `getMyHandlerR`、 `MyOtherHandlerR` は `handleMyOtherHandlerR` となる)。

ここまでで、どんな関数を書けば良いかわかったので、次は型注釈がどのようになるべきかについて理解していきましょう。

### 戻り値の型

簡単なハンドラ関数を見てみましょう。

```haskell
mkYesod "Simple" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<h1>This is simple|]
```

この戻り値の型には2つの要素 `Handler`、`Html` があります。これらについてより詳細に分析してみましょう。

#### ハンドラモナド

`Widget` 型のように `Handler` データ型は Yesod ライブラリのどこにも定義されていません。代わりにライブラリには以下のデータ型があります。

```haskell
data HandlerT site m a
```

`WidgetT` と同様に3つの引数を持ちます。`m` はベースモナド、`a` はモナド値、`site` はファウンデーションデータ型です。各アプリケーションは `site` をそのアプリケーションのファウンデーションデータ型、`m` を `IO` に制限した `Handler` シノニムを定義します。もし仮に、アプリケーションのファウンデーションが `MyApp` であれば、つまり次のようになるでしょう。

```haskell
type Handler = HandlerT MyApp IO
```

サブサイトを書く時にベースモナドを変更することがありますが、それ以外では `IO` を使います。

`HandlerT` モナドはユーザリクエストに関する情報 (例: クエリ文字列パラメータ) へのアクセス方法の提供や、レスポンス (例: レスポンスのヘッダ) の変更など、他にも多くのことができます。Yesod ソースコードの大半がこのモナドとなっているでしょう。

さらに `MonadHandler` と呼ばれる型クラスがあります。`HandlerT` と `WidgetT` はこの型クラスのインスタンスとなっているため、両方のモナドでは共通の関数が数多く使われています。
もし API ドキュメントの中に `MonadHandler` があれば、その関数は `Handler` 関数で使うことができる関数だということを思い出してください。

#### Html

この型について驚くようなことは何もありません。この関数は `Html` データ型で表される HTML コンテンツを返します。しかし、それが HTML レスポンスのみしか返せないのであれば Yesod は明らかに役に立ちません。CSS、Javascript、JSON、画像、そしてより多くのものをレスポンスとして返したいのです。その際、どんなデータ型を返してあげれば良いのだろうか？という疑問が生じると思います。

レスポンスを生成するためには、2つの情報を知る必要があります。それはコンテンツタイプ (例えば `text/html` や `image/png`) と、バイト列へのシリアライズ化の方法です。これは `TypedContent` データ型で表現されます。

```haskell
data TypedContent = TypedContent !ContentType !Content
```

また任意のデータ型を `TypedContent` に変換するための型クラスがあります。

``` haskell
class ToTypedContent a where
    toTypedContent :: a -> TypedContent
```

`Html`、`Value` (JSON を表す。aeson パッケージが提供している)、`Text`、`()` (空レスポンスを表す) など、よく利用する多くのデータ型は、この型クラスのインスタンスになっています。

### 引数

先ほどの例に戻りましょう。

```haskell
mkYesod "Simple" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<h1>This is simple|]
```

あらゆるルートがこの `HomeR` ほど単純ではありません。例えば、前に見た `PersonR` のルートをもう一度確認してみましょう。人物の名前をハンドラ関数に渡す必要があります。このやり方は率直でとてもわかりやすいものです。以下に例を示します。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import           Data.Text (Text)
import qualified Data.Text as T
import           Yesod

data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
/person/#Text PersonR GET
/year/#Integer/month/#Text/day/#Int DateR
/wiki/*Texts WikiR GET
|]

getPersonR :: Text -> Handler Html
getPersonR name = defaultLayout [whamlet|<h1>Hello #{name}!|]

handleDateR :: Integer -> Text -> Int -> Handler Text -- text/plain
handleDateR year month day =
    return $
        T.concat [month, " ", T.pack $ show day, ", ", T.pack $ show year]

getWikiR :: [Text] -> Handler Text
getWikiR = return . T.unwords

main :: IO ()
main = warp 3000 App
```

各ルートの引数は指定された並び順のダイナミックな断片の型になります。また、 戻り値として `Html` と `Text` のどちらも利用可能なことに注意してください。

## ハンドラ関数

コードのほとんどは `Handler` モナドに書くのでとても重要です。より深く理解するためにもう少し時間を費やしましょう。この章の残りでは `Handler` モナドでよく使う関数について簡単に紹介したいと思います。セッション関数についてはセッションの章で解説するため、今回は特に説明しません。

### アプリケーション情報

アプリケーション全体の情報を返し、個々のリクエストについては何の情報もあたえない関数が数多くあります。それらをいくつか紹介します。

#### getYesod

アプリケーションのファウンデーション値を返します。もし, ファウンデーションに設定値を保存していれば、この関数を多用することになるでしょう。(一応 `Control.Monad.Reader` の `ask` を使うこともできます。`getYesod` は単にその型を制限したエイリアスです。)

#### getUrlRender

型安全URLを `Text` に変換する URLレンダリング関数を返します。Hamlet を使えば Yesod が代わりにこの関数を呼び出してくれますが、直接呼び出す必要もたまにはあります。

#### getUrlRenderParams

`getUrlRender` の亜種です。型安全URLとクエリ文字列パラメータのリストを `Text` に変換します。この関数は必要なすべてのパーセントエンコーディングを行います。

### リクエスト情報

現在のリクエストで欲しい情報は、リクエストされたパス、クエリ文字列パラメータ、そして `POST` されたフォームデータが一般的です。リクエストされたパスは既に説明したようにルーティングで取り扱われます。他の2つはフォームモジュールを使って適切に取り扱います。

また、時々データを raw 形式で取得したいときがあります。この目的のために Yesod には `YesodRequest` データ型とリクエストを回収するための `getRequest` 関数があります。この関数により GET パラメーターの完全なリスト、クッキー、優先言語へのアクセスが可能になります。さらに `lookupGetParam`、`lookupCookie`、`languages` のような検索用の便利な関数があります。POST パラメータへの raw アクセスに関しては `runRequestBody` を利用しましょう。

リクエストヘッダのようにさらに多くの raw データが必要な場合 `waiRequest` を使ってウェブアプリケーションインタフェース (WAI) リクエスト値にアクセスできます。詳細については付録の WAI を参照してください。

### ショート

ハンドラ関数の処理を即座に終了し、結果をユーザに返します。

#### redirect

リダイレクトレスポンスをユーザ (303レスポンス) に送ります。違うレスポンスコード (例えば、永続的な301リダイレクト) にしたい場合は `redirectWith` を利用します。

<div class=yesod-book-notice>
Yesod は HTTP/1.1 クライアントには 303 レスポンス、そして HTTP/1.0 クライアントには 302 レスポンスを返します。このひどい実装は HTTP の仕様によるものです。
</div>

#### notFound

404 レスポンスを返します。存在しないデータベース値をユーザがリクエストした場合などで便利です。

#### permissionDenied

特定のエラーメッセージとともに403レスポンスを返します。

#### invalidArgs

有効でない引数のリストと一緒に400レスポンスを返します。

#### sendFile

指定されたコンテンツタイプでファイルシステムからファイルを送信します。これは下地の WAI ハンドラが `sendfile` システムコールに最適化できるため、静的ファイルを送る方法として好まれます。静的ファイルを送るために `readFile` を使う必要はありません。

#### sendResponse

200ステータスコードと共に通常のレスポンスを返します。即座にレスポンスが返るため、深くネストしたコードから抜け出す必要があるときに便利です。また `ToTypedContent` の任意のインスタンスが利用できます。

#### sendWaiResponse

低レベルにおいて raw WAI レスポンスを送りたい場合に利用します。これはストリーミングレスポンスやサーバーが送るイベントのような技術を作るときに便利です。

### レスポンスヘッダ

#### setCookie

クライアントにクッキーをセットします。この関数は有効期限を指定する代わりに、分単位でクッキーの持続時間を指定します。次のリクエストまでは `lookupCookie` を使っても、このクッキーは見えないことを覚えておいてください。

#### deleteCookie

クライアントにクッキーを消去するように伝えます。これもまた `lookupCookie` を使っても、次のリクエストまで変更は反映されません。

#### addHeader

任意のレスポンスヘッダを設定します。

#### setLanguage

ユーザの優先言語を設定します。設定した内容は `languages` 関数の結果として現れます。

#### cacheSeconds

何秒間このレスポンスをキャッシュするかを制御する Cache−Control ヘッダを設定します。これは [vanish on your server](http://www.varnish-cache.org/) を利用している場合に特に便利です。

#### neverExpires

Expires ヘッダを2037年に設定します。これはリクエストパスがそれと関連したハッシュ値を持っている場合のように、有効期限が決して切れないコンテンツと共に用いられます。

#### alreadyExpired

Expires ヘッダを過去に設定します。

#### expiresAt

Expires ヘッダを指定した日時に設定します。

## 入出力とデバッグ

`HandlerT` と `WidgetT` モナドトランスフォーマーは両方とも多くの型クラスのインスタンスになっています。ここで重要な型クラスは `MonadIO` と `MonadLogger` です。`MonadIO` はファイルからの読み込みのようなハンドラ内における任意の `IO` アクションが可能となります。使い方は、呼び出しの先頭に `liftIO` を追加するだけです。

`MonadLogger` は組み込みのロギングシステムを追加します。どのメッセージがログに記録されるか、どこにログが記録されるかを含み、システムをカスタマイズするための多くの方法があります。デフォルトではログは標準出力、開発においてはすべてのメッセージ、製品版においては警告とエラーがログに記録されます。

ログ取得時に、ソースコードのどこでログが行われているか知りたい時があります。このために `MonadLogger` は自動的にソースコードの位置をログメッセージに挿入するいくつもの便利なテンプレート Haskell 関数を提供しています。これらの関数は `$logDebug`、`$logInfo`、`$logWarn`、`$logError` です。これらの関数を使った例を見てみましょう。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Control.Exception (IOException, try)
import           Control.Monad     (when)
import           Yesod

data App = App
instance Yesod App where
    -- This function controls which messages are logged
    shouldLogIO App src level =
        return True -- good for development
        -- level == LevelWarn || level == LevelError -- good for production

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = do
    $logDebug "Trying to read data file"
    edata <- liftIO $ try $ readFile "datafile.txt"
    case edata :: Either IOException String of
        Left e -> do
            $logError "Could not read datafile.txt"
            defaultLayout [whamlet|An error occurred|]
        Right str -> do
            $logInfo "Reading of data file succeeded"
            let ls = lines str
            when (length ls < 5) $ $logWarn "Less than 5 lines of data"
            defaultLayout
                [whamlet|
                    <ol>
                        $forall l <- ls
                            <li>#{l}
                |]

main :: IO ()
main = warp 3000 App
```

## クエリ文字列とハッシュフラグメント

`redirect` のような URL っぽいものに対して機能するいくつもの関数を見てきました。これらの関数はすべて型安全URLで機能しますが、他にはどのようなものと機能するのでしょうか？`RedirectUrl` と呼ばれる型クラスは、ある型をテキスト形式の URL に変換するメソッドを備えます。これは型安全URL、テキスト形式のURL、そしてさらに2つの特別なインスタンスが定義されています。。

1. URLとクエリ文字列パラメータのキー/値ペアのリストのタプル
1. ハッシュフラグメントを URL の最後に追加するときに使う `Fragment` データ型

これら2つのインスタンスによって、追加の情報を型安全URLに加えることができます。どうやって利用するか、例を見てみましょう。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text        (Text)
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/      HomeR  GET
/link1 Link1R GET
/link2 Link2R GET
/link3 Link3R GET
/link4 Link4R GET
|]

instance Yesod App where

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Redirects"
    [whamlet|
        <p>
            <a href=@{Link1R}>Click to start the redirect chain!
    |]

getLink1R, getLink2R, getLink3R :: Handler ()
getLink1R = redirect Link2R -- /link2
getLink2R = redirect (Link3R, [("foo", "bar")]) -- /link3?foo=bar
getLink3R = redirect $ Link4R :#: ("baz" :: Text) -- /link4#baz

getLink4R :: Handler Html
getLink4R = defaultLayout
    [whamlet|
        <p>You made it!
    |]

main :: IO ()
main = warp 3000 App
```

もちろん以下のように Hamlet テンプレート内でハッシュを直接 URL の後に追加できるのでこれは多くの場合不要です。

```haskell
<a href=@{Link1R}#somehash>Link to hash
```

## まとめ

ルーティングとディスパッチは間違いなく Yesod の心臓部です。型安全URLはここで定義され、大部分のコードは `Handler` モナド内部に記述します。この章では Yesod において最も重要な中心的概念について説明しました。これらをを適切に理解することが重要です。

この章ではまた、後に扱ういくつものより複雑な Yesod に関するトピックについても触れました。しかし、ここまでで学んだ知識だけでもかなり洗練されたウェブアプリケーションを書くことが可能なはずです。

## 本書のコード

- [Example01.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch07/Example01.hs)
- [Example02.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch07/Example02.hs)
- [Example03.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch07/Example03.hs)
- [Example04.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch07/Example04.hs)
- [Example05.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch07/Example05.hs)
- [Example06.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch07/Example06.hs)
- [Example07.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch07/Example07.hs)