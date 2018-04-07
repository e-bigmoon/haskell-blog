---
title: Routing and Handlers
date: 2018/04/07
---

## Routing and Handlers

Yesod をモデル-ビュー-コントローラフレームワークとして見れば、ルーティングとハンドラはコントローラに対応します。
次の2つは他のウェブ開発環境において利用される2つのルーティング方法です。

- 名称に基づいてディスパッチする方法。例えば PHP や ASP はこの方法で動作します
- 正規表現に基づいたルートのパースを行うような、中央ルート処理関数を持つ方法。Django や Rails はこの方法を採用しています。

Yesod は原則的には後者の技術に近いのですが、実際には大きな違いが存在するします。
Yesod は正規表現を利用する代わりに、ルートの断片に基づいてマッチングを行います。
また片方向のルート-ハンドラのマッピングを使う代わりに, Yesod は中間データ型 (ルートデータ型, あるいは型安全URLと呼ばれる) と 双方向の変換関数を生成します。

このようなより発展的なシステムを手動でコーディングすることは、冗長でありかつエラーを起こしやすいです。

そのため Yesod はルートに特化したドメイン特化言語 (DSL) を定義し、この DSL を Haskell コードに変換するためのテンプレート Haskell 関数を提供します。
この章ではルーティング宣言に関する構文を説明し、コード生成がどのように行われるかを確認します。
そして、ルーティングとハンドラ間の相互作用について説明します。

## Route Syntax

Yesod のアプローチはルート宣言を既存の構文に無理やり埋め込むのではなく、ルートのためだけに設計された簡潔な構文を利用することです。
これは、コードの読み書きを簡単にするだけでなく Yesod の経験が全く無い場合でも、アプリケーションのサイトマップのように、十分に理解しやすいという利点があります。

この構文の基本的な例は以下の通りです。

``` haskell
/             HomeR     GET
/blog         BlogR     GET POST
/blog/#BlogId BlogPostR GET POST

/static       StaticR   Static getStatic
```

これから、ルート宣言において何が起こっているかについて詳細を完全に説明します。

### Pieces

Yesod がリクエストを得たときに最初に行うことの1つは、リクエストされたパスを断片に分割することです。
断片は全てのフォワードスラッシュ部分でトークン化されます。
例えば以下のようにです。

``` haskell
toPieces "/" = []
toPieces "/foo/bar/baz/" = ["foo", "bar", "baz", ""]
```

末尾スラッシュや2重スラッシュ ("/foo//bar//") のような問題のある状況が起こり得ることに気づくでしょう。
Yesod は正規化された URL を持つことを推奨します。
もしユーザが末尾スラッシュあるいは2重スラッシュ付きでURLをリクエストすれば、それらは自動的に正規化された URL にリダイレクトされます。
その結果、1つのリソースにつき1つの URL だけ持つことが保証されるため、検索順位に良い影響を与えます。

これが意味することは、URL の正確な構造に悩む必要がないということです。
パス断片を安全なものとして考えることができ、Yesod が自動的にスラッシュの挿入を制御し、問題の生じ得る文字をエスケープしてくれます。

ところでどのようにパスが断片に分割され再結合されるか詳細な制御方法について知りたい場合は Yesod 型クラスの章における `cleanPath` や `joinPath` メソッドを再確認してみると良いでしょう。

### Types of Pieces

ルートを宣言する場合、処理に当たって3種類の型が存在します。

#### Static

これは URL と正確に一致する必要のある通常の文字列です。

#### Dynamic Single

これは単一の断片 (2つのスラッシュの間の) ですが、ユーザが入力した値を表し、ページリクエストに対し付属的なユーザ入力を受け取るための主要な方法です。
これらの断片は hash (#) で始まり、そのあとにデータ型が続き、それは `PathPiece` のインスタンスでなければなりません。

#### Dynamic multi

Dynamic Single と同じですが、URL 断片を複数受け取ることができます。
また、リソースパターンでは常に断片が最後に来る必要があります。
それはデータ型に続いてアスタリスク (\*) を 指定し、さらにデータ型は `PathMultiPiece` のインスタンスでなければなりません。
複数の断片は他の2つと比べて少し変わっていますが、静的表現のファイル構造や任意の階層を持つ wiki のような機能を実装するためにとても重要です。

Yesod 1.4 から dynamic multi を指定するために `+` が追加されました。
これは、C プロセッサでは `/*` 文字を組み合わせることで混乱が生じ得るため、とても重要です。

これから一度は書くような標準的なリソースパターンを見てみましょう。
一番簡単なものは、アプリケーションルートを `/` にするものです。同じように FAQ を `/page/faq` としたりする場合もあるでしょう。

例えばフィボナッチウェブサイトを作りたいと思ったら URL を `/fib/#Int` のように構築するでしょう。
しかし、これにはちょっとした問題があります。
我々はマイナスの値やゼロがアプリケーションに入力されたくありません。
幸運なことに型システムはこれをしっかりと防いでくれます。

```haskekkl
newtype Natural = Natural Int
instance PathPiece Natural where
    toPathPiece (Natural i) = T.pack $ show i
    fromPathPiece s =
        case reads $ T.unpack s of
            (i, ""):_
                | i < 1 -> Nothing
                | otherwise -> Just $ Natural i
            [] -> Nothing
```

1行目は不適切な入力を避けるために Int 型を単純な nwetype ラッパーで定義する。

`PathPiece` は2つのメソッドをもつ型クラスであることがわかります。
`toPathPiece` は `Text` に変換する以上のことは何も行いません。
`fromPathPiece` は `Text` をデータ型に変換しよう試みますが、この変換が不可能な場合は `Nothing` を返します。
このデータ型を使うことでハンドラ関数に自然数のみが与えられることを保証することができるため、型システムを使って境界問題に対抗することが可能となりました。

現実世界のアプリケーションにおいては、内部的にアプリケーション上有効でない　`Natural`　値を決して偶然的に作らないよう保証したいでしょう。
そのようにするためには [smart constructors](https://wiki.haskell.org/Smart_constructors) のような方法があります。
今回の例の目的とは少し違うので、コードに余分な情報を加えないことにします。

`PathMultiPiece` を定義することも同じくらい簡単です。
例えば、少なくとも2つ以上の階層をもつ Wiki を作りたいので、次のようなデータ型を定義しましょう。

``` haskell
data Page = Page Text Text [Text] -- 2 or more
instance PathMultiPiece Page where
    toPathMultiPiece (Page x y z) = x : y : z
    fromPathMultiPiece (x:y:z) = Just $ Page x y z
    fromPathMultiPiece _ = Nothing
```

### Overlap checking

Yesod はどの2つのルートも互いに重複する可能性のないことを標準で保証している。
そこで例えば次のようなルートを考えてみましょう。

``` haskell
/foo/bar   Foo1R GET
/foo/#Text Foo2R GET
```

`/foo/bar` が両方のルートに一致してしまうため、このルート宣言は重複しているものとして拒否されます。
しかし、重複を許したいと思うような2つのケースがあります。

1. データ型の定義から重複が決して起こらないことがわかっている場合。例えば、上の `Text` を `Int` で置き換えたとすると、存在するルートで重複するものが無いことはすぐにわかる。しかし、Yesod は今のところそのような分析はできません。
1. アプリケーションがどのように作動するかについて特別な知識を持っているため、そのような状況が決して起こらないとわかっているとき。例えば、 `Foo2R` ルートがパラメータ`bar` を受け取ることが絶対にできない場合などです。

エクスクラメーションマークをルートの始まりに追加することで重複チェックをオフにすることができます。
例えば次のコードは問題なく Yesod で動作します。

``` haskell
/foo/bar    Foo1R GET
!/foo/#Int  Foo2R GET
!/foo/#Text Foo3R GET
```

`#`、 `*`、`+` 文字の後のように、エクスクラメーションマークをどのパス断片の始めに置いても大丈夫ですが、新しい構文のほうが目的が明確なためより好まれます。

重複するルートが生ずる問題はわかりにくさです。
上の例では `foo/bar` は `Foo1R` と `Foo3R` のどちらにルーティングするべきでしょうか？
また `/foo/42` は `Foo2R` と `Foo3R` のどちらにルーティングするべきでしょうか？
Yesod の規則は単純で、最初のルートが優先されます。

### Resource name

それぞれのリソースパターンは関連した名前を持っています。
その名前はアプリケーションと結びついた型安全URLデータ型のためのコンストラクタとなるため、大文字で始まらなければなりません。
慣習として、これらのリソース名はすべて大文字の R としています。
これは強要するものは何もなく、それは単に共通の小技みたいなものです。

コンストラクタの正確な定義はそれが結びついているリソースパターンに依存しています。
単一断片あるいは複数断片で用いられるどのデータ型であっても、それがデータ型の引数となります。
これはアプリケーションにおいて型安全 URL 値と有効な URL の間における1対1の関係があります。

これは、すべての値が機能するページであることを意味するのではなく、単に有効な可能性のある URL であることを意味します。
例えば `PersonR "Michael"` という値はデータベースに Michael がなければ有効なページにはたどり着かないでしょう。

ここで実例を挙げましょう。
`PersonR` という名前の `/person/#Text`、 `YearR` という名前の `/year/#Int`、 `FaqR` という名前の `/page/faq` というリソースパターンがあれば、だいたい次のようなルートデータ型になるであろう.

``` haskell
data MyRoute = PersonR Text
             | YearR Int
             | FaqR
```

ユーザが `year/2009` をリクエストすれば Yesod はそれを `YearR 2009` に変換します。
`person/Michael` は `PersonR "Michael"` となり、 `page/faq` は `FaqR` なります。
一方で `/year/two-thousand-nine`、 `/person/michael/snoyman`、 `/page/FAQ` はコードを見なくても404エラーになることがわかります。

### Handler specification

リソースを宣言する際の最終問題はそれらがどのように処理されるかです。
Yesod には次の3つの選択肢があります。

- 与えられたルートにおけるすべてのリクエストメソッドのためのシングルハンドラ関数
- 与えられたルートにおけるそれぞれのリクエストメソッドのためのセパレートハンドラ関数。その他のリクエストメソッドは 405 Method Not Allowed を返す
- サブサイトに受け流す

はじめの2つは簡単に使えます。
シングルハンドラ関数は `/page/faq FaqR` のようなリソースパターンとリソース名が1行で書かれています。
この場合、ハンドラ関数は `handleFaqR` と命名されなければなりません。

それぞれのリクエストメソッドに対応するためのセパレートハンドラも同じ書き方ですが、リクエストメソッドのリストが必要になります。
例えば `/person/#String PersonR GET POST DELETE` というようにです。
この場合、 `getPersonR`、`postPersonR`、 `deletePersonR` という3つのハンドラ関数を定義する必要があります。

サブサイトは非常に素晴らしいのですが Yesod のかなり複雑な話題です。
サブサイトの書き方については後にしますが、それらを使うことはそれほど難しくありません。
最も一般的なサブサイトは静的サブサイトで、それはアプリケーションのために静的ファイルを提供します。
`/static` から静的ファイルを受け取るためには次のようなリソース行が必要となります。

``` haskell
/static StaticR Static getStatic
```

この行では `/static` は単にURL構造のどこから静的ファイルを受け取るかということを言っています。
static という言葉自体には何も魔法の力はなく `/my/nondynamic/files` などに簡単に変更できます。

次の単語である `StaticR` はリソース名です。
次の2つの単語はサブサイトを使っていることを指定するものです。
`Static` はサブサイトのファウンデーションデータ型で、 `getStatic` はマスターファウンデーションデータ型から `Static` 値を取り出すための関数です。

今はサブサイトの詳細にはあまり深入りしないようにしましょう。
scaffolded サイトの章で、静的サブサイトについての詳細を確認します。

## Dispatch

一度ルートを特定すれば Yesod は URL ディスパッチに関するあらゆる厄介な詳細を引き受けてくれます。
そのためには、確実に適切なハンドラ関数を与えていさえすれば良いのです。
サブサイトのルートに関してはハンドラ関数を書く必要はありませんが、他の2つに関しては書く必要があります。
命名規則については既に説明しました (`MyHandlerR GET` は `getMyHandlerR`、 `MyOtherHandlerR` は `handleMyOtherHandlerR` となる)。
これで書く必要のある関数が分かったので、次は型注釈がどのようになるべきかについて理解していきましょう。

### Return Type

簡単なハンドラ関数を見てみましょう。

``` haskell
mkYesod "Simple" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<h1>This is simple|]
```

この戻り値の型には2つの要素 `Handler`、 `Html` があります。
これらについてより詳細に分析してみましょう。

#### Handler monad

`widget` 型のように `Handler` データ型は Yesod のどのライブラリにおいても定義されていません。
代わりにライブラリはデータ型を与えます。

``` haskell
data HandlerT site m a
```

`WidgetT` と同様に3つの引数を持ちます。
`m`はベースモナド、 `a`はモナド値、 `site` はファウンデーションデータ型です。
各アプリケーションは `site` をそのアプリケーションのファウンデーションデータ型、 `m` を `IO` というような `Handler` シノニムを定義します。
もしファウンデーションが `MyApp` であれば、つまり次のようになるでしょう。

``` haskell
type Handler = HandlerT MyApp IO
```

サブサイトを書く時に下地のモナドを変更することがありますが、そうでない場合は `IO` を使います。

`HandlerT` モナドはユーザリクエスト情報 (例: クエリ文字列パラメータ) へのアクセス方法の提供や、レスポンス (例: レスポンスのヘッダ) を変更できるようにしたり、他にも多くのことができます。
Yesod ソースコードの大半がこのモナドとなっているでしょう。

さらに `MonadHandler` と呼ばれる型クラスがあります。
`HandlerT` と `WidgetT` はこの型クラスのインスタンスとなっているため、両方のモナドでは共通の関数が数多く使われています。
もし API 文章の中に `MonadHandler` があれば、その関数は `Handler` 関数で使うことができる関数だということを思い出してください。

### Html

この型について驚くべきことは何もありません。
この関数は `Html` データ型で表される HTML コンテンツを返します。
しかし、それが HTML レスポンスのみしか返せないのであれば Yesod は明らかに役に立ちません。
CSS、Javascript、JSON、画像、そしてより多くのものをレスポンスとして返したいのです。
その際、どんなデータ型を返してあげれば良いのだろうか？という疑問が生じると思います。

レスポンスを生成するためには、2つの情報を知る必要があります。
それはコンテンツタイプ (例えば `text/html`、 `image/png`) と、バイト列へのシリアライズ化の方法です。
これは `TypedContent` データ型で表現されます。

``` haskell
data TypedContent = TypedContent !ContentType !Content
```

また任意のデータ型を `TypedContent` に変換できる型クラスがあります。

``` haskell
class ToTypedContent a where
    toTypedContent :: a -> TypedContent
```

`Html`、`Value` (JSON を表す。aeson パッケージが提供している)、`Text`、 `()` (空レスポンスを表す) など、よく利用する多くのデータ型は、この型クラスのインスタンスになっています。

## Arguments

先ほどの例に戻りましょう。

``` haskell
mkYesod "Simple" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<h1>This is simple|]
```

あらゆるルートがこの `HomeR` ほど単純ではありません。
例えば、前に見た `PersonR` のルートをもう一度確認してみましょう。
person の名前はハンドラ関数に渡す必要があります。
このやり方は率直でとてもわかりやすいものです。
以下に例を示します。

``` haskell
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

引数は各ルートで dynamic pieces の型の値を指定された順番で持ちます。
また、 戻り値として `Html` と `Text` のどちらも利用可能なことに注意してください。

## The Handler functions

大部分のコードは `Handler` モナドにあるため、それをより理解するために時間を費やすことは重要です。
この章の残りでは `Handler` モナドにある最も一般的な関数について手短に紹介します。
セッション関数についてはセッションの章で解説するため、今回は特に説明しません。

### Application Information

アプリケーション全体の情報を返し、個々のリクエストについては何の情報も返さない関数が数多くあります。
それらをいくつか紹介します。

#### getYesod

アプリケーションのファウンデーション値を返します。
もし, ファウンデーションに設定値を保存していれば、この関数を多用することになるでしょう。
(一応 `Control.Monad.Reader` の `ask` を使うこともできる。 `getYesod` は単にその型を特殊化したものです。)

#### getUrlRender

URLレンダリング関数を返し、それは型安全URLを `Text` に変換します。
Hamlet を使えば Yesod が代わりにこの関数を呼び出してくれますが、直接呼び出す必要もたまにはあります。

#### getUrlRenderParams

`getUrlRender` の異なるバージョンです。
型安全 URL とクエリ文字列パラメータのリストを `Text` に変換します。
この関数は必要なすべてのパーセントエンコーディングを行います。

### リクエスト情報

現在のリクエストで欲しい情報はリクエストされたパス、クエリ文字列パラメータ、そして `POST` されたフォームデータが一般的です。
リクエエストされたパスは既に説明したようにルーティングで取り扱われます。
他の2つはフォームモジュールを使って適切に取り扱います。

また、時々データを生の形で得る必要があります。
このために Yesod では `YesodRequest` データ型とそれを回収するための `getRequest` 関数が使えます。
これにより GET パラメーターの完全なリスト、クッキー、優先言語へのアクセスが可能になります。
さらに `lookupGetParam`、 `lookupCookie`、 `language` のような探索のための便利な関数があります。
POST パラメータへの生アクセスに関しては `runRequestBody` を利用しましょう。
リクエストヘッダのようにさらに多くの生データが必要な場合 `waiRequest` を使って Web Application Interface(WAI) リクエスト値にアクセスできます。
より詳細については付録の WAI を参照してください。

### Short Circuiting

即座にハンドラ関数の処理を終了し、結果をユーザに返します。

#### redirect

リダイレクトレスポンスをユーザ (303レスポンス) に送ります。
違うレスポンスコード (例えば、パーマネント301リダイレクト) にしたい場合は `redirectionWith` を利用します。

Yesod は HTTP/1.1 クライアントには 303 レスポンス、そして HTTP/1.0 クライアントには 302 レスポンスを返します。
このひどい実装は HTTP の仕様によるものです。

#### notFound

404 レスポンスを返します。
ユーザが存在しないデータベース値をリクエストした場合によく利用します。

#### permissionDenied

特定のエラーメッセージとともに403レスポンスを返します。

#### invalidArgs

有効でない引数のリストと一緒に400レスポンスを返します。

#### sendFile

特定のコンテンツ型を持つファイルシステムからファイルを送信します。
これは下地の WAI ハンドラがこれを `sendfile` システムコールに最適化できるため、静的ファイルを送る方法として好まれます。
静的ファイルを送るための `readFile` は必要ありません。

#### sendResponse

200ステータスコードと共に通常のレスポンスを返します。
即座にレスポンスが返るため、深くネストしたコードから抜け出す必要があるときに便利です。
また `ToTypedContent` の任意のインスタンスが利用できます。

#### sendWaiResponse

低レベルにおいて生の WAI レスポンスを送りたい場合に利用します。
これはストリーミングレスポンスやサーバーが送るイベントのような技術を作りたい場合に利用します。

### Response Headers

#### setCookie

クライアントにクッキーをセットします。
この関数は有効期限を取る代わりに、分単位でクッキーの期間を取ります。
次のリクエストまでは `lookupCookie` を使っても、このクッキーは見えないことを覚えておいてください。

#### deletCookie

クライアントにクッキーを消去するように伝えます。
これもまた `lookupCookie` を使っても、次のリクエストまで変更は反映されません。

#### setHeader (現在は addHeader)

任意のレスポンスヘッダを設定する。

#### setLanguage

ユーザの優先言語を設定する。
これは `language` 関数の結果として出現する。

#### cacheSeconds

何秒間このレスポンスをキャッシュするかを制御する Cache−Control ヘッダを設定します。
これは [vanish on your server](http://www.varnish-cache.org/) を用いている場合に特に利用価値があります。

#### neverExpires

Expires ヘッダを2037年に設定します。
これはリクエストパスがそれと関連したハッシュ値を持っている場合のように、有効期限が決して切れないコンテンツと共に用いられます。

#### alreadyExpired

Expires ヘッダを過去に設定します。

#### expiresAt

Expires ヘッダを特定の日/時間に設定します。

## I/O and debugging

`HeaderT` と `WidgetT` モナドトランスフォーマは両方とも多くの型クラスのインスタンスとなっています。
ここで重要な型クラスは `MonadIO` と `MonadLogger` です。
`MonadIO` によってファイルからの読み込みのようなハンドラ内における任意の `IO` アクションが可能となります。
使い方としては、呼び出しの先頭に `liftIO` を追加するだけで良いです。

`MonadLogger` は内蔵されたログシステムを追加します。
どのメッセージがログに記録されるか、どこにログが記録されるかを含み、システムをカスタマイズするための多くの方法があります。
デフォルトではログは標準出力、開発においてはすべてのメッセージ、製品版においては警告とエラーがログに記録されます。

しばしばログを行ってるとき、ソースコードのどこでログが行われているか知りたいかもしれません。
このために `MonadLogger` は自動的にソースコードの位置をログメッセージに挿入するいくつもの便利なテンプレート Haskell 関数を提供しています。
これらの関数は `$logDebug`、 `$logInfo`、 `$logWarn`、 `$logError` です。
これらの関数を理解する例を見て見ましょう。

``` haskell
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
    shouldLogIO App src level = return $
        True -- good for development
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
            $logError $ "Could not read datafile.txt"
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

## Query string and hash hragments

`redirect` のように URL のようなものに対して機能するいくつもの関数を見てきました。
これらの関数はすべて型安全 URL と共に機能しますが、他にはどのようなものと機能するのでしょうか？
`RedirectUrl` と呼ばれる型クラスがあり、それはある型をテキスト形式の URL に変換するメソッドを備えます。
これは型安全URL、テキスト形式のURL、そして2つの特別なインスタンスを含みます。

1. URLとクエリ文字列パラメータのキー/値ペアのリストのタプル
1. ハッシュ断片を URL の最後に追加するために使われる `Fragment` データ型

これら2つのインスタンスによって、付加的な情報を型安全URLに加えることができます。
これらがどのように使われるかについて、例を見てみましょう。

``` haskell
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

もちろん Hamletテンプレートにおいてはハッシュを直接 URL の後に追加できるのでこれは多くの場合不要です。

``` haskell
<a href=@{Link1R}#somehash>Link to hash
```

## Summary

ルーティングとディスパッチは間違いなく Yesod のコアです。
型安全URLはここから定義され、大部分のコードは `Handler` モナド内部に記述されます。
この章では Yesod において最も重要な中心的概念について説明しました。
したがって、それを適切に理解することが重要です。

この章ではまた、後に扱ういくつものより複雑な Yesod に関するトピックについても触れました。
しかし、ここまでで学んだ知識だけでもかなり洗練されたウェブアプリケーションを書くことが可能なはずです。