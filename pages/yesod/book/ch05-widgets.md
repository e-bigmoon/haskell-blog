---
title: ウィジェット
date: 2018/09/14
---

Web 開発において、 HTML、CSS、Javascript といったクライアントサイド技術の調整は非常に厄介な問題です。なぜなら、これらのコンポーネントのうち CSS の style タグは `<head>` タグの内部、Javascript の script タグは `</body>` タグの直前、そして HTML は `<body>` タグの内部というように、ページの異なる場所に適切に配置する必要があるためです。CSS と Javascript を別々のファイルに保存するのであれば、この問題については気にしなくても大丈夫です！

実際のところ、単一ページを作成するのであれば、かなりよく機能します。なぜなら、構造 (HTML)、スタイル (CSS)、ロジック (Javascript) を分離できるからです。しかし、簡単に合成できるモジュラなコードを書こうと思うと、これら構成要素を別々に調節しなければならないので悩ましく思います。ウィジェットは、この問題に対する Yesod の解決策です。それは、jQuery のようなライブラリを確実に1度だけ読み込みたいという問題も解決できます。

Yesod の4つのテンプレート言語 Hamlet、Cassius、Lucius、Julius は何らかの出力を作るための低レベルなツールです。ウィジェットは、それらのテンプレート言語を相互を合成するための接着剤のようなものです。

## 概要

```haskell
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

上記のプログラムは、以下の HTML を生成します (読みやすいようにインデントを追加しています)。

```html
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

## ウィジェットとは何だろう？

考え方によっては HTML 文書は単にネストされたタグの塊と捉えることもできるでしょう。多くの HTML 生成ツールのアプローチはタグの階層を定義して進めます。ここで、関数を適切なタイミングで呼び出し、ナビゲーションバーが階層の適切な部分に挿入されるような "プラグ&プレイ" で動作するナビゲーションバーを表示するページコンポーネントを記述することを考えてみてください。

これは、表面的な HTML 生成ツールで対処できない問題です。ナビゲーションバーは HTML に加え、複数の CSS と Javascript で構成されているでしょう。ナビゲーションバーを生成する関数を呼び出すタイミングでは既に `<head>` タグがレンダリングされているため、CSS 宣言のために `<style>` タグを新たに追加するには遅すぎます。通常の方法では、ナビゲーションバーを HTML、CSS、Javascript の3つの部分に分解し、さらに常にこれら3つ全てを呼び出していることを確認する必要があります。

この問題に対して、ウィジェットは異なるアプローチを取ります。モノリシックなタグのツリーとして HTML 文書を扱うのではなく、以下に列挙するようなページにおける多くの異なるコンポーネントとして扱います。

- タイトル
- 外部スタイルシート
- 外部スクリプト
- CSS 宣言
- Javascript コード
- 任意の `<head>` コンテンツ
- 任意の `<body>` コンテンツ

それぞれのコンポーネントで異なるセマンティクスを持ちます。例えば、タイトルは常に1つですが、外部スクリプトやスタイルシートは複数記述することができます。しかし実際のところ、外部スクリプトとスタイルシートは1回のみインクルードされるべきです。一方で、 任意のヘッドとボディコンテンツには制限がありません (5つのダミーテキストを持ちたい等々の人がいるかもしれないので)。

ウィジェットの仕事は、セマンティクスが異なるこれらのコンポーネントを手放さずに、異なるウィジェットを組み合わせるために適切なロジックを適用することです。例えば、複数のタイトルが現れる場合は最後のタイトル以外を無視し、外部スクリプトとスタイルシートから重複をフィルタリングして、ヘッドとボディコンテンツを連結するような具合です。

## ウィジェットの作り方

ウィジェットを使いこなすために、まずは使ってみましょう。最も一般的な方法は `ToWidget` 型クラスと `toWidget` メソッドを利用することです。これによりシェイクスピアテンプレートを直接 `Widget` に変換できます。Hamlet コードは `<body>`、 Julius スクリプトは `<script>`、 Cassius と Lucius は `<style>` タグ内にそれぞれ出現します。

<div class="yesod-book-notice">
実際にはデフォルトの動作を上書き可能なので、スクリプトとスタイルコードを別ファイルに保存することもできます。scaffolded サイトでは自動的にこの機能が有効になります。
</div>

しかし `head` に現れる必要のある `<meta>` タグを追加したい場合はどうでしょうか？また Javascript を `head` の代わりに `body` に現れて欲しい場合はどうしますか？この問題を解決するために Yesod は2つの型クラス `ToWidgetHead` と `ToWidgetBody` を用意しています。以下は `<script>` タグを最終的にどこに挿入するか、きめ細かに制御するための例です。

```haskell
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
main = warp 3000 App
```

`toWidgetHead` は `toWidgetBody` の後に呼ばれていますが、 `<script src=/included-in-head.js>` タグは生成される HTML の最初に現れることに注意してください。

さらに、特定の種類のウィジェットを作るための関数がいくつもあります。

### setTitle

HTML にページタイトルを挿入します。

### toWidgetMedia

`toWidget` と同じように機能しますが、どのメディアに適用されるかに関する追加的なパラメータを必要とします。例えば、印刷用のスタイルシートを作成する場合などで利用します。

### addStylesheet

`<link>` タグにより、外部スタイルシートへの参照を追加します。型安全URL を引数に取ります。

### addStyleSheetRemote

`addStylesheet` と同じですが、通常の URL を引数に取ります。Google の jQuery UI CSS ファイルなどの CDN にホストされたファイルを参照するために利用されます。

### addScript

外部スクリプトに対し `<script>` タグによる参照を追加します。型安全URL を利用します。

### addScriptRemote

`addScript` と同じですが、通常の URL を利用します。Google の jQuery のような CDN にホストされたファイルを参照するために利用されます。

## ウィジェットを組み合わせる

ウィジェットの全体的なアイデアは構成性を高めることです。HTML、CSS、Javascript の個々をより複雑なものに組み合わせ、これら大きな実体を組み合わせることで完全なページを作り上げます。これらは `Widget` の `Monad` インスタンスを用いれば自然に機能します。つまり、それぞれを1つにまとめ上げるために do 記法を使います。

```haskell
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

<div class="yesod-book-notice">
`Widget` の `Monoid` インスタンスが定義されてるので、`mconcat` や `Writer` モナドを利用することも可能です。経験上、単に do 記法を使うことが最も簡単かつ自然です。
</div>

## ID の生成

もし、本当にコードの再利用を行うとすると、名前衝突の問題に悩まされるでしょう。スタイリングに影響を与える "foo" という同一のクラス名が2つの補助的なライブラリがあったらどうしますか？このような可能性を避けるためには `newIdent` 関数を利用しましょう。この関数はハンドラの中でユニークな識別子を生成します。

```haskell
getRootR = defaultLayout $ do
    headerClass <- newIdent
    toWidget [hamlet|<h1 .#{headerClass}>My Header|]
    toWidget [lucius| .#{headerClass} { color: green; } |]
```

## whamlet

以下のコードは、フッターとなる Hamlet テンプレートを埋め込んだ、Hamlet テンプレートの例です。

```hamlet
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

これは、フッターがただの HTML であれば機能しますが、何らかのスタイルを適用したい場合はどうなるでしょうか？そういう時は、 Hamlet をウィジェットに変換することで簡単にフッターをカスタマイズできます。

```haskell
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

しかし、ここで Hamlet テンプレートは Hamlet テンプレートしか埋め込みできないという問題が発生します。すなわち Hamlet はウィジェットについては何も知らないのです。この問題を解決するためには `whamlet` を利用します。whamlet は通常の Hamlet と同じく変数展開 (`#{...}`)、型安全URL (`@{...}`) などの構文を持ちます。しかし、埋め込み (`^{...}`) は `Widget` を取り、最終的な結果も `Widget` になります。whamlet を使うためには単に次のようにします。

```haskell
page =
    [whamlet|
        <p>This is my page. I hope you enjoyed it.
        ^{footer}
    |]
```

別のファイルにテンプレートを保存しておきたい場合は `whamletFile` を使います。

<div class="yesod-book-notice">
scaffolded サイトではさらに便利な関数 `widgetFile` があります。これは Lucius、Cassius、Julius ファイルを自動的にインクルードできます。この関数の詳細については、scaffolding の章で説明します。
</div>

## 型

これまで型注釈を避けてきたことに気付いたかもしれません。それぞれのウィジェットは `Widget` 型です。しかし Yesod ライブラリを見渡しても `Widget` 型の定義はどこにも見当りません。一体それは何なのでしょうか？

Yesod は `Widget` に非常に類似した型 `data WidgetT site m a` を定義します。このデータ型は、モナドトランスフォーマー (***monad transformer***) です。最後2つの引数は、それぞれ下地のモナドとモナド値であり、`site` パラメータはそれぞれのアプリケーションで定義されるファウンデーション型です。ファウンデーション型はアプリケーション毎に異なるため、ライブラリがあらゆるアプリケーションで機能するような `Widget` 型は定義できないのです。

その代わりに、Template Haskell を使って `mkYesod` 関数が型シノニムを生成します。ファウンデーションデータ型が `MyApp` なら `Widget` シノニムは次のように定義されるでしょう。

```haskell
type Widget = WidgetT MyApp IO ()
```

モナド値が `()` となっているのがわかります。それは、ウィジェット値が最終的には捨て去られてしまうからです。`IO` は標準的なベースモナドであり、たいていの場合はこのモナドが使われます、唯一の例外はサブサイト (***subsite***) を記述する場合ですが、サブサイトはより発展的な話題なので、後の章で説明します。

一度 `Widget` 型を理解してしまえば、前のコード例に型注釈を追加するのは簡単です。

```haskell
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

ハンドラ関数により深く踏み込む時に `HandlerT` と `Handler` 型について、同じような状況に出くわすと思います。

## ウィジェットを使う

これらの素晴らしいウィジェット型を手に入れたのは良いのですが、どのようにそれらをユーザが相互作用できるものにするのでしょうか？最も一般的に使用されている関数は `defaultLayout` です。この関数の型は、基本的には `Widget -> Handler Html` となるでしょう。

`defaultLayout` は実際には型クラスのメソッドであり、それぞれのアプリケーション毎に上書きできます。defaultLayout 関数によって Yesod アプリケーションのレイアウトを作ります。しかし、 `defaultLayout` の内部で、どのように `Widget` をアンラップすれば良いのか？という疑問が残ります。答えは `widgetToPageContent` にあるので (単純化された) 型を見てみましょう。

```haskell
data PageContent url = PageContent
    { pageTitle :: Html
    , pageHead  :: HtmlUrl url
    , pageBody  :: HtmlUrl url
    }
widgetToPageContent :: Widget -> Handler (PageContent url)
```

これは必要なものに近づきつつあります。今や title、head、body を構成する HTML への直接アクセスが可能です。この時点で Hamlet を使ってサイトのレイアウトとともにそれら全てを組み合わせて単一のドキュメントを作成できます。そして、ユーザに表示するため `withUrlRenderer` 関数を使って Hamlet の結果を HTML ファイルに変換します。次の例で、この処理を確認してみましょう。

```haskell
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

まだ悩ましい `style` タグの問題が残っているので、それに関する問題をいくつか列挙します。

- Lucius や Cassius とは違い、正しさについてコンパイル時チェックを行いません。
- 今回の例がかなり単純なことを考慮すると、より複雑なものでは文字のエスケープ問題が発生する可能性があります。
- 今回は1つではなく、2つの style タグがあります。一方は `myLayout` により生成され、もう一方はウィジェット中のスタイルセットに基づいて `pageHead` で生成されます

この問題に対処するためのもうひとつのトリックとして `widgetToPageContent` を呼び出す前に最終調整を行うというやり方があります。実際にはかなり単純で、単に do 記法を再度使うだけです。

```haskell
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

## ハンドラ関数を使う

ハンドラについてはまだあまり説明できていません。しかし、少し考えるとウィジェットの中でハンドラ関数をどのように使えば良いのだろうか？という疑問が浮かびます。例えば、ウィジェットが `lookupGetParem` を用いてクエリ文字列パラメータを探す必要がある場合はどうでしょう？最初の答えは `handlerToWidget` 関数であり、これは `Handler` アクションを `Widget` 変換できますが、多くの場合これは不要です。`lookupGetParem` の型注釈を考えてみましょう。

```haskell
lookupGetParam :: MonadHandler m => Text -> m (Maybe Text)
```

この関数は `MonadHandler` の**任意の**インスタンスで適切に動作します。便利なことに `Widget` は `MonadHandler` のインスタンスとして定義されているので、 多くのコードが `Handler` でも `Widget` でも実行可能なことを意味します。そして、もし明確に `Handler` から `Widget` に変換する必要がある場合にだけ `handlerToWidget` を利用すれば良いでしょう。

<div class="yesod-book-notice">
この点が Yesod バージョン1.1とそれ以前のものを比べた際の重大な躍進です。以前は `MondHandler` 型クラスが全くありませんでした。そのため、全ての関数を `handlerToWidget` ではなく、明示的に `lift` を使って変換する必要があったのです。新システムは使い易さだけなく、かつて採用されていたあらゆる奇怪なモナド変換のトリックを避けることができます。
</div>

## まとめ

それぞれのページにおける基本的な構成要素はウィジェットです。HTML、CSS、Javascript 個々のスニペットは多相的な `toWidget` 関数を使ってウィジェットに変換できます。do 記法を使えば、これら個々のウィジェットを組み合わせて大きなウィジェットを作成でき、最終的にはページの全てのコンテンツを含むことができます。

これらのウィジェットをアンラップする作業は、たいてい全てのページに統一的な見た目を適用するために `defaultLayout` 関数で行われます。

## 本書のコード

- [Example01.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch05/Example01.hs)
- [Example02.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch05/Example02.hs)
- [Example03.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch05/Example03.hs)
- [Example04.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch05/Example04.hs)