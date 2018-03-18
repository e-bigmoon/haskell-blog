---
title: Widgets
date: 2018/03/18
---

## Widgets

ウェブ開発における最も困難なもののうちの1つは、3つのクライアント側の技術 (HTML, CSS, Javascript) を調節しなければならないことです。
さらに悪いことに、これらのコンポーネントのうち、CSS の style タグは`<head>`タグの内部、Javascript の script タグは`<body>`の閉じタグの前、そしてHTMLは`<body>`タグの内部といったページの異なる場所に置かなければなりません。
もし、CSS と Javascript を異なるファイルに保存したい場合は特に気にする必要はありませんが。

現実的には、単一ページを作成する場合はかなりよく機能します。なぜなら、構造 (HTML)、スタイル (CSS)、ロジック (Javascript) を分離できるからです。
しかし、簡単に合成可能なモジュラーコードを作成したい場合、これら構成要素を別々に調節しなければならないのは悩ましく思います。
ウィジェットは、この問題に対する Yesod の解決策です。
それは、jQuery のようなライブラリを1度だけ読み込みたいという問題も解決できます。

Yesod の4つのテンプレート言語 Hamlet、Cassius、Lucius、Julius はアウトプットを構築するための構文を提供します。
ウィジェットは、それらのテンプレート言語を相互に合成可能とするための接着剤として働きます。

## 概要

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod

data App = App
mkYesod "App" [parseRoutes|
/ HomeR GET
|]
instance Yesod App

getHomeR = defaultLayout $ do
    setTitle "My Page Title"
    toWidget [lucius| h1 { color: green; } |]
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
    toWidget
        [julius|
            $(function() {
                $("h1").click(function(){
                    alert("You clicked on the heading!");
                });
            });
        |]
    toWidgetHead
        [hamlet|
            <meta name=keywords content="some sample keywords">
        |]
    toWidget
        [hamlet|
            <h1>Here's one way of including content
        |]
    [whamlet|<h2>Here's another |]
    toWidgetBody
        [julius|
            alert("This is included in the body itself");
        |]

main = warp 3000 App
```

これは次のHTMLコードを生成します (読みやすいようにインデントを追加しています)。

``` html
<!DOCTYPE html>
<html>
  <head>
    <title>My Page Title</title>
    <meta name="keywords" content="some sample keywords">
    <style>h1{color:green}</style>
  </head>
  <body>
    <h1>Here's one way of including content</h1>
    <h2>Here's another</h2>
    <script>
      alert("This is included in the body itself");
    </script>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js">
    </script><script>
      $(function() {
        $('h1').click(function() {
          alert("You clicked on the heading!");
        });
      });
    </script>
  </body>
</html>
```

## What's in a Widget?

かなり表面的なレベルにおいては、HTML 文書は単にネストされたタグの固まりと考えることもできるでしょう。
大抵の HTML 生成ツールのアプローチはタグの階層を定義して進めます。
しかし、関数を適切なタイミングで呼び出し、ナビゲーションバーが階層の適切な部分に挿入されるような "プラグ&プレイ" で動作するナビゲーションバーを表示するページコンポーネントを記述することを考えてみましょう。

これは、表面的なHTML生成ツールで対処できない問題です。
ナビゲーションバーは HTMLに加え、複数の CSS と Javascript で構成されているでしょう。
ナビゲーションバーを生成する関数を呼び出すタイミングでは既に`<head>`タグがレンダリングされているため、CSS 宣言のために `<style>` タグを新たに付加するには遅すぎます。
通常の方法では、ナビゲーションバーを HTML、CSS、Javascript の3つの部分に分解し、さらに常にこれら3つ全てを呼び出していることを確認する必要があります。

これに対して、ウィジェットは異なる方法を取ります。HTML 文書をタグでできた一枚岩の木として扱うのではなく、以下に列挙するようなページにおける多くの異なるコンポーネントとして扱います。

- タイトル
- 外部スタイルシート
- 外部スクリプト
- CSS 宣言
- Javascript コード
- 任意の `<head>` コンテンツ
- 任意の `<body>` コンテンツ

異なるコンポーネントは異なる意味を持ちます。
例えば、タイトルは常に1つですが、外部スクリプトやスタイルシートは複数記述することができます。
しかし実際のところ、外部スクリプトとスタイルシートは1回のみインクルードされるべきです。
一方で、 任意のヘッドとボディコンテンツには制限がありません (5つのダミーテキストを持ちたい等々の人がいるかも知れない)。

ウィジェットはこれらの分離したコンポーネントを手放さずに、異なるウィジェットを組み合わせるための適切なロジックを適用しています。
例えば、複数のタイトルが現れる場合は最後以外を無視し、外部スクリプトとスタイルシートから重複をフィルタリングして、ヘッドとボディコンテンツを連結するような具合です。

## Constructing Widgets

ウィジェットを使いこなすために、まずは使ってみましょう。
最も一般的な方法は `ToWidget` 型クラスと `toWidget` メソッドを利用することです。
これによりシェイクスピアテンプレートを直接 `Widget` に変換することが可能になります。
Hamlet コードは `<body>`、 Julius スクリプトは `<script>`、 Cassius と Lucius は `<style>` タグにそれぞれ出現します。

実際にはデフォルトの動作を上書き可能なので、スクリプトとスタイルコードを別ファイルに作成することもできます。
scaffolded サイトは自動的にこの機能を提供しています。

しかし `head` に現れる必要のある `<meta>` タグを追加したい場合はどうでしょうか？
また Javascript を `head` の代わりに `body` に現れて欲しい場合はどうするか？
この問題を解決するために Yesod は2つの追加的な型クラス `ToWidgetHead` と `ToWidgetBody` を用意しています。
以下は `script` タグが最終的にどこに挿入されるか、きめ細かに制御するための例です。

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/      HomeR  GET
|]

instance Yesod App where

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "toWidgetHead and toWidgetBody"
    toWidgetBody
        [hamlet|<script src=/included-in-body.js>|]
    toWidgetHead
        [hamlet|<script src=/included-in-head.js>|]

main :: IO ()
main = warp 3001 App
```

`toWidgetHead` は `toWidgetBody` の後に呼ばれていますが、後の `<script>` タグは生成されたHTMLの最初に現れることに注意してください。

さらに、特定の種類のウィジェットを作るための関数がいくつもあります。

### setTitle

HTML にページタイトルを挿入します。

### toWidgetMedia

`toWidget` と同じように機能しますが、どのメディアに適用されるかに関する追加的なパラメータを必要とします。
例えば、印刷用のスタイルシートを作成する場合などで利用します。

### addStylesheet

`<link>` タグにより、外部スタイルシートに参照を追加します。
型安全 URL を引数に取ります。

### addStyleSheetRemote

`addStylesheet` と同じですが、通常の URL を引数に取ります。
Google の jQuery UI CSS ファイルなどのような CDN にホストされたファイルを参照するために利用されます。

### addScript

外部スクリプトに対し `<script>` タグによる参照を追加します。
型安全 URL を利用します。

### addScriptRemote

`addScript` と同じですが、通常の URL を利用します。
Google の jQuery ファイルなどのような CDN にホストされたファイルを参照するために利用されます。

## Combining Widgets

ウィジェットの全体的なアイデアは構成性を高めることです。
HTML、CSS、Javascript の個々をより複雑なものに組み合わせ、これら大きな実体を組み合わせ完全なページを構築することができます。
これらは `Widget` の `Monad` インスタンスを用いれば自然に機能します。
つまり、個々を1つにまとめ上げるために do 記法を用いることができます。

``` haskell
myWidget1 = do
    toWidget [hamlet|<h1>My Title|]
    toWidget [lucius|h1 { color: green } |]

myWidget2 = do
    setTitle "My Page Title"
    addScriptRemote "http://www.example.com/script.js"

myWidget = do
    myWidget1
    myWidget2

-- or, if you want
myWidget' = myWidget1 >> myWidget2
```

`Widget` の `Monoid` インスタンスが定義されてるため、構築のために `mconcat` や `Writer` モナドを利用することも可能です。
経験上、単に do 記法を用いるのが最も簡単かつ自然です。

## Generate IDs

もし本当にコードの再利用を行いたい場合、名前衝突の問題に悩まされるでしょう。
スタイリングに影響を与える "foo" という同一のクラス名が2つの補助的なライブラリがあるとしましょう。
このような可能性は避けるために `newIdent` 関数を利用します。
この関数は、ハンドラの中でユニークな識別子を生成します。

``` haskell
getRootR = defaultLayout $ do
    headerClass <- newIdent
    toWidget [hamlet|<h1 .#{headerClass}>My Header|]
    toWidget [lucius| .#{headerClass} { color: green; } |]
```

## whamlet

例えば、フッターを表現するために他の Hamlet ファイルを埋め込む、かなり標準的な Hamlet ファイルがあるとしましょう。

``` hamlet
page =
    [hamlet|
        <p>This is my page. I hope you enjoyed it.
        ^{footer}
    |]

footer =
    [hamlet|
        <footer>
            <p>That's all folks!
    |]
```

これは、フッターが古臭い HTML であれば機能しますが、何らかのスタイルを加えるとどうなるでしょうか？
この場合 Hamlet をウィジェットに変換することで簡単にフッターをカスタマイズできます。

``` haskell
footer = do
    toWidget
        [lucius|
            footer {
                font-weight: bold;
                text-align: center
            }
        |]
    toWidget
        [hamlet|
            <footer>
                <p>That's all folks!
        |]
```

しかし、ここで Hamlet テンプレートは Hamlet テンプレートのみ埋め込み可能という問題が発生します。
すなわち Hamlet はウィジェットについては何も知らないのです。
この問題を解決するために `whamlet` を利用します。
whamlet は通常の Hamlet と同じく変数展開 (`#{...}`)、型安全URL (`@{...}`) などの構文を持ちます。
しかし、埋め込み (`^{...}`) は `Widget` を引数取り、最終的な結果も `Widget` になります。
whamlet を使うために単に次のようにすれば良いでしょう。

``` haskell
page =
    [whamlet|
        <p>This is my page. I hope you enjoyed it.
        ^{footer}
    |]
```

テンプレートを別ファイルにしたい場合のために `whamletFile` も存在します。

scaffolded site ではさらに便利な関数 `widgetFile` が存在し、これは Lucius、Cassius、Julius ファイルを自動的にインクルードできます。
この関数の詳細については、scaffolding の章で説明します。

## Types

これまで型注釈を避けてきたことには気付いたかもしれません。
その単純な答えは、それぞれのウィジェットは `Widget` 型であるからです。
しかし Yesod ライブラリを見渡しても `Widget` 型の定義はどこにも見当りません。
一体それは何なのでしょうか？

Yesod は widget に非常に類似した型 `data WidgetT site m a` を定義します。
このデータ型は、モナドトランスフォーマーです。
最後2つの引数は、それぞれモナドとモナド値であり、`site` パラメータは個々のアプリケーションにおける特定のファウンデーション型です。
この型は全てそれぞれのサイトによって異なるため、ライブラリがあらゆるアプリケーションで機能するような単一の `Widget` を定義することは不可能なのです。

代わりに、テンプレートHaskellの `mkYesod` 関数が型シノニムを生成します。
ファウンデーションデータ型が `MyApp` の場合 `Widget` シノニムは次のように定義されるでしょう。
代わりに、テンプレートHaskellの `mkYesod` 関数が型シノニムを生成します。ファウンデーションデータ型が `MyApp` の場合 `Widget` シノニムは次のように定義される。

``` haskell
type Widget = WidgetT MyApp IO ()
```

モナド値が `()` となっているのがわかります。
それは、ウィジェット値が最終的には捨て去られてしまうからです。
`IO` は標準的なベースモナドであり、ほとんど全ての場合に用いられます、
唯一の例外はサブサイトを記述する場合ですが、サブサイトはより上級の話題なので、後の章で説明します。

一度 `Widget` 型について理解できれば、型注釈を前のコード例に加えることは簡単です。

``` haskell
footer :: Widget
footer = do
    toWidget
        [lucius|
            footer {
                font-weight: bold;
                text-align: center
            }
        |]
    toWidget
        [hamlet|
            <footer>
                <p>That's all folks!
        |]

page :: Widget
page =
    [whamlet|
        <p>This is my page. I hope you enjoyed it.
        ^{footer}
    |]
```

ハンドラ関数により深く踏み込む際には `HandlerT` と `Handler` 型について、同じような状況となるでしょう。

## Using Widgets

これらの素晴らしいウィジェットデータ型を手に入れたことは良いことですが、どのようにそれらをユーザが相互作用できるものにするのでしょうか？
最も一般的に使用されている関数は `defaultLayout` で、基本的には `Widget -> Handler Html` の型注釈を持ちます。

`defaultLayout` は実際には型クラスのメソッドであり、それぞれのアプリケーション毎に上書きできます。
defaultLayout 関数は Yesod アプリケーションがどのようにテーマ化されるかを決定します。
しかし `defaultLayout` を利用する際、どのように `Widget` をアンラップすれば良いのか？という疑問が残ります。
答えは `widgetToPageContent` にあるので (単純化された) 型を見てみましょう。

``` haskell
data PageContent url = PageContent
    { pageTitle :: Html
    , pageHead :: HtmlUrl url
    , pageBody :: HtmlUrl url
    }
widgetToPageContent :: Widget -> Handler (PageContent url)
```

これは必要なものに近づきつつあります。
今や title、head、body を構成する HTML へ直接アクセスが可能です。
この点において Hamlet を用いてサイトのレイアウトとともにそれら全てを組み合わせて単一の文書を作成できます。
そして Hamlet の結果をユーザに見せる準備ができた実際の HTML ファイルにするために `withUrlRenderer` 関数を使います。
次の例で、この処理を確認してみましょう。

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod

data App = App
mkYesod "App" [parseRoutes|
/ HomeR GET
|]

myLayout :: Widget -> Handler Html
myLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle pc}
                    <meta charset=utf-8>
                    <style>body { font-family: verdana }
                    ^{pageHead pc}
                <body>
                    <article>
                        ^{pageBody pc}
        |]

instance Yesod App where
    defaultLayout = myLayout

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>Hello World!
    |]

main :: IO ()
main = warp 3000 App
```

まだ1つ悩ましい `style` タグの問題があるので、それに関する問題をいくつか列挙します。

- Lucius や Cassius とは違い、正しさについてコンパイル時チェックを行いません
- 今の例がかなり単純なことを考慮すると、より複雑なものでは文字エスケーピング問題が発生する可能性があります
- 今回は1つではなく、2つの style タグがあります。一方は `myLayout` により生成され、もう一方はウィジェット中のスタイルセットに基づいて `pageHead` で生成されます

これに対処するためのもうひとつのトリックとして `widgetToPageContent`を用いる前に、最終調整を行うというやり方があります。
それは、実際にはかなり単純であり、単に do 記法を再度使う方法です。

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod

data App = App
mkYesod "App" [parseRoutes|
/ HomeR GET
|]

myLayout :: Widget -> Handler Html
myLayout widget = do
    pc <- widgetToPageContent $ do
        widget
        toWidget [lucius| body { font-family: verdana } |]
    withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle pc}
                    <meta charset=utf-8>
                    ^{pageHead pc}
                <body>
                    <article>
                        ^{pageBody pc}
        |]

instance Yesod App where
    defaultLayout = myLayout

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>Hello World!
    |]

main :: IO ()
main = warp 3000 App
```

## Using handler functions

ハンドラについてはまだあまり説明できていません。しかし、少し考えるとそれらのハンドラ関数をウィジェットの中でどのように使えば良いのだろうか？という疑問が浮かびます。
例えば、ウィジェットが `lookupGetParem` を用いてクエリ文字列パラメータを探す必要がある場合はどうでしょう？
最初の答えは `handlerToWidget` 関数であり、これは `handler` アクションを `Widget` 変換できますが、多くの場合これは不要です。
`lookupGetParem` の型注釈を考えてみましょう。

``` haskell
lookupGetParam :: MonadHandler m => Text -> m (Maybe Text)
```

この関数は `MonadHandler` の任意のインスタンスで適切に動作します。
便利なことにウィジェットは `MonadHandler` のインスタンスでもあり、これは大部分のコードが`Handler` でも `Widget` でも実行可能なことを意味します。
そして、もし明確に `Handler` から `Widget` に変換する必要がある場合には常に `handlerToWidget` を利用すれば良いでしょう。

この点が Yesod バージョン1.1とそれ以前のものに比べるた際の重大な躍進です。
以前は `MondHandler` 型クラスが全くありませんでした。
そのため、全ての関数を `handlerToWidget` ではなく、明示的に `lift` を用いて変換する必要があったのです。
新システムは使い易いだけなく、かつて採用されていたあらゆる奇怪なモナド変換のトリックを避けることができます。

## Summary

それぞれのページにおける基本的な構成要素はウィジェットです。
HTML、CSS、Javascript 個々のスニペットは多相的な `toWidget` 関数を用いてウィジェットに変換できます。
do 記法を用いればこれら個々のウィジェットを組み合わせて大きなウィジェットを作成でき、最終的にはページの全ての内容を含ませることができます。

これらのウィジェットをアンラップする作業は、たいてい全てのページに統一的な見た目を適用するために defaultLayout 関数で行われます。
