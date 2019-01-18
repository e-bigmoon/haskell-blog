---
title: シェイクスピア テンプレート
date: 2019/1/18
---

Yesod では HTML, CSS, Javascript を生成するための標準的な方法として、テンプレート言語のシェイクスピアファミリーを採用しています。シェイクスピアでは共通の構文を持ち、以下の原則に従います。

- ベース言語 (Html, Css, Javascript) への干渉をできる限り少なくしながら、便利な機能を提供します。
- 妥当なコンテンツ (well-formed content) であることをコンパイル時に保証します。
- 静的型安全性は [XSS攻撃(cross-site scripting)](http://en.wikipedia.org/wiki/Cross-site_scripting) の防止に非常に役立ちます。
- 型安全URLを利用することで、可能な場合は常に展開されたリンクに対して自動的にバリデーションを行います。

Yesod とこれらのテンプレート言語の間には本質的な繋がりやその他の制限は何もありません。そのため、それぞれは互いに独立して利用することができます。本章の前半で、テンプレート言語の構文等について説明し、後半では Yesod アプリケーション開発でテンプレート言語の利用方法を述べます。

## 概要

シェイクスピアには4つの主要な言語があります。Hamlet は HTML、Julius は Javascript、Cassius と Lucius は両方とも CSS のためのテンプレート言語です。Hamlet と Cassius は空白によって識別される形式となっているため、インデントを用いてネストを表現します。それに対して Lucius は CSS のスーパーセットなので、通常の CSS で使われる波括弧の形式でネストを表します。Julius は Javascript を作り出すための単なるパススルー言語です。唯一追加される機能は変数展開 (variable interpolation) だけです。

<div class="yesod-book-notice">
Cassius は単に Lucius の代替構文です。内部では両者とも同じ処理エンジンを利用していますが、 Cassius のファイルは前処理の段階でインデントを波括弧に変換します。そのため、どちらを利用するかは純粋に構文の好みになります。
</div>

### Hamlet (HTML)

```hamlet
$doctype 5
<html>
    <head>
        <title>#{pageTitle} - My Site
        <link rel=stylesheet href=@{Stylesheet}>
    <body>
        <h1 .page-title>#{pageTitle}
        <p>Here is a list of your friends:
        $if null friends
            <p>Sorry, I lied, you don't have any friends.
        $else
            <ul>
                $forall Friend name age <- friends
                    <li>#{name} (#{age} years old)
        <footer>^{copyright}
```

### Lucius (CSS)

```lucius
section.blog {
    padding: 1em;
    border: 1px solid #000;
    h1 {
        color: #{headingColor};
        background-image: url(@{MyBackgroundR});
    }
}
```

### Cassius (CSS)

上の Lucius の例と同じです。

```cassius
section.blog
    padding: 1em
    border: 1px solid #000
    h1
        color: #{headingColor}
        background-image: url(@{MyBackgroundR})
```

### Julius (Javascript)

Julius の文字列展開は他の2つとは少し異なります。これは、セキュリティの観点からとても重要なことです。通常、全ての展開された値は妥当な JSON となっているため、XSS 攻撃を防ぐことができます。しかし、`rawJS` 関数は文字列をダブルクォートで囲まないため、利用する際は気をつけてください。

信頼できないのであれば、どんな入力に対しても絶対に `rawJS` 関数を使ってはいけません。例えば、ユーザが任意の入力を行えるフォームで使ってしまったら、それだけで脆弱性が発生します。

```haskell
-- 型クラスをインポートすることで rawJS 関数をインポートします
import Text.Julius (RawJS (..))
```

```julius
$(function(){
    $("section.#{rawJS sectionClass}").hide();
    $("#mybutton").click(function(){document.location = "@{SomeRouteR}";});
    ^{addBling}
});
```

## 型

構文に入る前に色々な型を見てみましょう。最初に「型は XSS 攻撃の防止に役立つ」と述べました。例えば、誰かの名前を表示する次のような HTML テンプレートがあったとしましょう。

```hamlet
<p>Hello, my name is #{name}
```

<div class="yesod-book-notice">
`#{...}` はシェイクスピアでは、変数展開を意味します。
</div>

さて、`name` 変数の値を挿入するためにはどうすれば良いと思いますか？さらにそのデータ型は何であるべきでしょうか？よくある方法は `Text` の値をそのまま挿入することです。しかし、そうすると `name` の値が次のような場合に問題が起こります。

```html
<script src='http://nefarious.com/evil.js'></script>
```

このとき `<` が `&lt;` になるように `name` の値をエンティティエンコードしたいのですが、どうすれば良いでしょうか。

すぐに思い浮かぶのは、単に埋め込まれる全てのテキストをエンティティエンコードするという方法です。しかし、もし、別の処理によって既に HTML が生成されていたとしたら何が起こるでしょうか？例えば、 Yesod のウェブサイトのすべての Haskell コードスニペットは適切な `span` タグで囲むことで色付けされています。仮に、全てをエンティティエスケープしたとすれば、コードスニペットはほとんど読めない状態になってしまうでしょう。

そういった事態を避けるために `Html` データ型を使います。`Html` 型の値を生成するために2種類のAPIがあります。`ToMarkup` 型クラスは `String` と `Text` の値を `Html` の値に変換する `toHtml` 関数を提供します。 `toHtml` 関数は自動的にエンティティエスケープを行います。これは先ほどの `name` の例で求めていた処理です。また、コードスニペットのような例では `preEscapedToMarkup` 関数を利用すると良いでしょう。

Hamlet (HTML シェイクスピア言語) の変数展開では、値に対して自動的に `toHtml` が適用されます。そのため `String` を展開する場合はエンティティエスケープされますが、 `Html` の場合は何も変更されません。そのため、コードスニペットの例では `#{preEscapedToMarkup myHaskellHtml}` のようにして展開すれば期待通りの結果が得られます。

<div class="yesod-book-notice">
`Html` データ型は `toHtml` や `preEscapedToMarkup` と一緒に blaze-html パッケージで提供されています。そのため Hamlet は他の全ての blaze-html パッケージと相互作用することが可能になり、 blaze-html の値を生成するための一般的解決策にもなります。さらには blaze-html の素晴らしいパフォーマンスの恩恵を得ることもできます。
</div>

同様に `CSS型 と ToCSS クラス`, `Javascript型 と ToJavascript クラス` があります。これらは意図せず HTML を壊してしまうようなコードの混入を防ぐため、コンパイル時にサニティーチェックを行います。

<div class="yesod-book-notice">
CSS にテンプレート言語を採用するその他の利点としては、色や単位についての補助的なデータ型があることです。例えば、以下のような例です。

```lucius
.red { color: #{colorRed} }
```

さらなる詳細については Haddock ドキュメントを参照してください。
</div>

## 型安全URL

おそらく Yesod における最も面白い機能は型安全URLです。この機能はシェイクスピアから直接扱うことができます。使い方は変数展開とほとんど同じで、ハッシュ (#) の代わりにアットマーク (@) を利用するだけです。構文は後で確認することにして、まずは直感的に理解しましょう。

まず、次のような2つのルートを持つアプリケーションがあるとします。`http://example.com/profile/home` はトップページで `http://example.com/display/time` は現在の時間を表示します。ここで、トップページから time ページにリンクしたいとき、 URL を構築するためには3つの異なる方法が考えられます。

1. 相対リンク: ***../display/time***
1. ドメイン無しの絶対リンク: ***/display/time***
1. ドメイン有りの絶対リンク: ***http://example.com/display/time***

どの方法もそれぞれ問題があります。1つ目の方法はどちらかのURLが変化することでリンク切れになってしまいますし、全ての状況に適しているというわけではありません。例えば RSS や Atom フィードには絶対URLが必要です。2つ目は1つ目の方法よりも変化に対し柔軟ですが、 RSS や Atom の問題は残ったままです。3つ目は今言った問題の全てを解決できますが、ドメイン名が変わるごとに全ての URL を更新する必要が生じます。そんなこと、そう頻繁には起こらないだろうと思うかもしれませんが、開発からステージングそして最終の本番サーバへの移行を考えてみてください。

しかしより重要なことはそれぞれの方法において、ひとつ非常に大きな問題があるということです。それは、仮にルートを変更したとしてもコンパイラは壊れたリンクに対して警告をしないという点です。誤植が大混乱を招くように、これは最悪な状況だということは言うまでもありません。

型安全URLの目的は人間の代わりにコンパイラにできる限り多くのチェックを行わせることです。これを可能にするための最初のステップとしては、コンパイラが理解できない単純な文字列から離れ、正しく定義されたデータ型に変更することです。今回の単純なアプリケーションではルートを直和型で表現します。

```haskell
data MyRoute = Home | Time
```

テンプレートに /display/time のようなリンクを記述する代わりに `Time` コンストラクタを利用します。しかし、そうすると HTML はデータ型ではなくテキストでできているため、これらの値をテキストに変換する方法が必要となります。その変換を行う関数をURLレンダリング関数 (URL rendering function) と呼ぶことにしましょう。簡単な例を以下に示します。

```haskell
renderMyRoute :: MyRoute -> Text
renderMyRoute Home = "http://example.com/profile/home"
renderMyRoute Time = "http://example.com/display/time"
```

<div class="yesod-book-notice">
URLレンダリング関数は実際にはこれより少しだけ複雑です。なぜなら、クエリ文字列パラメータを追加し、コンストラクタのレコードを扱い、より賢くドメイン名を処理する必要があるためです。しかし、実際にはそんな事を考えなくても大丈夫です。Yesod はレンダリング関数を自動生成してくれます。唯一指摘すべきこととしてはクエリ文字列を扱うために、型注釈が実際にはもうちょっとだけ複雑になるということです。

```haskell
type Query = [(Text, Text)]
type Render url = url -> Query -> Text
renderMyRoute :: Render MyRoute
renderMyRoute Home _ = ...
renderMyRoute Time _ = ...
```
</div>

これで、レンダリング関数とテンプレートに埋め込まれた型安全URLについて、なんとなくわかったと思います。しかし、正確にはこれらはどうやって上手く動作するのでしょうか？テンプレート言語は `Html` (`CSS` や `Javascript`) の値を直接生成するのではなく、レンダリング関数を引数に取り HTML を作り上げる関数を生成します。このことをより詳細に確認するために Hamlet が背後でどのような処理を行っているか軽く覗いてみましょう。以下のテンプレートを考えます。

```hamlet
<a href=@{Time}>The time
```

これは、だいたい次のような Haskell コードに翻訳されます。

```haskell
\render -> mconcat ["<a href='", render Time, "'>The time</a>"]
```

## 構文

シェイクスピア テンプレート言語はどれも同じ展開構文となっているため、同じ方法で型安全URLを利用できます。各テンプレート言語の構文は対象となる言語 (HTML、CSS、Javascript) によって少しずつ違います。それぞれの言語を順番に見てみましょう。

## Hamlet 構文

Hamlet はシェイクスピアの中でも最も洗練されたものです。HTML を生成のための構文を提供するだけでなく、条件、ループ、maybe などの基本的な制御構造を備えます。

### タグ

どんな HTML テンプレート言語でも明らかにタグは重要です。Hamlet では、テンプレート言語を使いやすくするためにできる限り既存の HTML 構文に似せています。一番大きな違いはネストの表現に、閉じタグではなくインデントを採用している点です。そのため、次のような HTML は

```html
<body>
<p>Some paragraph.</p>
<ul>
<li>Item 1</li>
<li>Item 2</li>
</ul>
</body>
```

このような Hamlet コードに変換されます。

```hamlet
<body>
    <p>Some paragraph.
    <ul>
        <li>Item 1
        <li>Item 2
```

一般的に、この表記に一度慣れてしまえば通常の HTML よりわかりやすいのではないでしょうか。ただし、タグ前後の空白の扱いについては注意が必要です。例えば、次のような HTML を生成したいとしましょう。

```html
<p>Paragraph <i>italic</i> end.</p>
```

空白は "Paragraph" の後の部分や "end" の前の部分で保存されていて欲しいです。そのためには、2種類のエスケープ文字を用います。

```hamlet
<p>
    Paragraph #
    <i>italic
    \ end.
```

空白のエスケープ規則は実にシンプルです。

1. もし、行頭の文字が空白でなく、バックスラッシュであれば、そのバックスラッシュは無視されます。(注意: これは、その行における全てのタグが単なるテキストとして処理されてしまうということです)
2. もし、行末の文字がハッシュの場合それは無視されます。

あと、 Hamlet はコンテンツ内のエンティティをエスケープしません。そのため、既存の HTML のコピーがしやすくなります。したがって、上の例は次のように書くこともできます。

```hamlet
<p>Paragraph <i>italic</i> end.
```

Hamlet では最初のタグは自動的に閉じられることに注意してください。それに対して内側の "i" タグはそうではありません。どちらの方法もペナルティ無く自由に使うことができるため、好きな方を選択できます。しかし Hamlet において閉じタグを使う唯一の場面は、このようなインラインタグに限られるということを忘れないでください。通常のタグを閉じることはありません。

この表記で他に注意する点としては、最初のタグより後に出現するタグは ID やクラスについて、特別な処理を行わないということです。例えば、次の Hamlet スニペットは

```hamlet
<p #firstid>Paragraph <i #secondid>italic</i> end.
```

以下の HTML を生成します。

```html
<p id="firstid">Paragraph <i #secondid>italic</i> end.</p>
```

`i` タグが単純なテキストとして処理される一方で `p`タグは自動的に閉じられ、その属性が特別扱いを受ける点に注意してください。

### 展開

これまでに紹介したものは簡潔な HTML なので何の問題も起こっていませんが、まだ Haskell コードと相互作用できていません。どのようにして変数を受け渡せば良いのでしょうか？それは単純に展開を利用します。

```hamlet
<head>
    <title>#{title}
```

ハッシュの後ろに波括弧のペアが続く表記は変数展開 (***variable interpolation***) です。上の例では、テンプレートが呼ばれたスコープで `title` 変数が利用できます。そのことは次のように言い直せます。Hamlet は変数が呼ばれた際に自動的にスコープ内の変数へアクセスします。そのため、特に変数を受け渡す必要はありません。

展開の中では関数適用、文字列、数値リテラル、修飾付きモジュールを利用することができます。また、処理をまとめるために丸括弧やドルマークを使うこともできます。展開の処理の最後に `toHtml` 関数が適用されます。これは `ToHtml` クラスの全てのインスタンスが展開可能だということを意味します。例えば、次のコードを考えてみましょう。

```haskell
-- Just ignore the quasiquote stuff for now, and that shamlet thing.
-- It will be explained later.
{-# LANGUAGE QuasiQuotes #-}
import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Char (toLower)
import Data.List (sort)

data Person = Person
    { name :: String
    , age  :: Int
    }

main :: IO ()
main = putStrLn $ renderHtml [shamlet|
<p>Hello, my name is #{name person} and I am #{show $ age person}.
<p>
    Let's do some funny stuff with my name: #
    <b>#{sort $ map toLower (name person)}
<p>Oh, and in 5 years I'll be #{show ((+) 5 (age person))} years old.
|]
  where
    person = Person "Michael" 26
```

目玉機能の型安全URLについてはどうでしょうか？それらはシャープの代わりにアットマーク (@) で始まる点を除けば、あらゆる面で変数展開と同じです。さらにキャレット (^) によって同じ型の別のテンプレートを埋め込むことが可能になります。次のコード例で確認しましょう。

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text)

data MyRoute = Home

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = "/home"

footer :: HtmlUrl MyRoute
footer = [hamlet|
<footer>
    Return to #
    <a href=@{Home}>Homepage
    .
|]

main :: IO ()
main = putStrLn $ renderHtml $ [hamlet|
<body>
    <p>This is my page.
    ^{footer}
|] render
```

さらに、クエリ文字列パラメータを受け入れるような URL 展開の変種があます。これは例えばページ番号をつけるようなレスポンスに適しています。`@{...}` を用いる代わりにクエスチョンマークを加え `@?{...}` とすることで、クエリ文字列があることを表します。与える値は2要素のタプルでなければなりません。第1要素が型安全URL、第2要素はクエリ文字列パラメータのリストのペアとなります。例えば、次のコードスニペットの例を見てみましょう。

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text, append, pack)
import Control.Arrow (second)
import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding (decodeUtf8)
import Blaze.ByteString.Builder (toByteString)

data MyRoute = SomePage

render :: MyRoute -> [(Text, Text)] -> Text
render SomePage params = "/home" `append`
    decodeUtf8 (toByteString $ renderQueryText True (map (second Just) params))

main :: IO ()
main = do
    let currPage = 2 :: Int
    putStrLn $ renderHtml $ [hamlet|
<p>
    You are currently on page #{currPage}.
    <a href=@?{(SomePage, [("page", pack $ show $ currPage - 1)])}>Previous
    <a href=@?{(SomePage, [("page", pack $ show $ currPage + 1)])}>Next
|] render
```

これは、次のようなHTMLを生成します。

```html
<p>You are currently on page 2.
<a href="/home?page=1">Previous</a>
<a href="/home?page=3">Next</a>
</p>
```

### 属性

前の最後の例では "a" タグに href 属性を追加しました。構文について詳しく見てみましょう。

- 属性の値にも変数展開が利用できます。
- HTML と同様に属性のイコール記号と値はオプションです。したがって `<input type=checkbox checked>` は完全に正しい書き方です。
- id と class 属性についてはそれぞれハッシュ (#) と ピリオド (.) を使います。つまり `<p #paragraphid .class1 .class2>` という感じです。
- 属性値を囲うための前後のクォートは省略可能ですが、途中でスペースを用いる場合は必要です。
- コロンを利用することでオプションの属性を追加できます。isChecked 変数の値が True の場合のみチェックボックスをチェックした状態にしたい場合 `<input type=checkbox :isChecked:checked>` とします。また、ある段落の文字色を赤色にしたい場合は `<p :isRed:style="color:red">` とします。(これはクラス名を使っても良いです。例えば、 `<p :isCurrent:.current>` は isCurrent が `True` の場合, `current` クラスをセットします)
- 任意のキーと値のペアもまた `*{...}` 構文を用いて展開が可能です。属性の場合の変数展開は値が String または Text のタプルか, タプルのリストである必要があります。例えば `attrs = [("foo", "bar")]` という値であれば展開可能です。

### 条件文

いつかはページ内に何らかのロジックを組み込みたいと思うでしょう。Hamlet の目的はロジックを可能な限り最小にすることで Haskell に大部分を任せることです。そのようなものとして `if`、 `elseif`、 `else` は非常に基本的な論理文です。

```hamlet
$if isAdmin
    <p>Welcome to the admin section.
$elseif isLoggedIn
    <p>You are not the administrator.
$else
    <p>I don't know who you are. Please log in so I can decide if you get access.
```

通常の展開と同じ規則が条件文にも当てはまります。

### Maybe

同様に Maybe 値を扱うための特別な記法があります。技術的には `if`、 `isJust`、 `fromJust` を使えば同じ処理を行えます。しかし、`maybe` を使うと、部分関数を避けることができるので、こちらの方が便利です。

```hamlet
$maybe name <- maybeName
    <p>Your name is #{name}
$nothing
    <p>I don't know your name.
```

単純な識別子に加え、コンストラクタやタプルなど、より複雑な値を左辺に使うことができます。

```hamlet
$maybe Person firstName lastName <- maybePerson
    <p>Your name is #{firstName} #{lastName}
```

右辺は展開と同じ規則に従い、変数や関数適用などが使えます。

### Forall

リストのループについてはどうでしょうか？こんな感じです。

```hamlet
$if null people
    <p>No people.
$else
    <ul>
        $forall person <- people
            <li>#{person}
```

### Case

パターンマッチは Haskell の重要な強みの1つです。直和型は多くの現実的な型をきれいにモデル化することができます。また、 `case` 文を利用すると安全にマッチさせることができます。どれにもマッチしない場合には、コンパイラが警告を出してくれます。Hamlet の case 文も、それと同様の力を持ちます。

```hamlet
$case foo
    $of Left bar
        <p>It was left: #{bar}
    $of Right baz
        <p>It was right: #{baz}
```

### With

文を短くするために `with` を利用します。基本的には長い式に対して、エイリアスを定義するために利用します。

```hamlet
$with foo <- some very (long ugly) expression that $ should only $ happen once
    <p>But I'm going to use #{foo} multiple times. #{foo}
```

### Doctype

最後はちょっとした糖衣構文の doctype 文です。
`dectype` は複数の異なるバージョンがサポートされていますが、モダンな Web アプリケーションでは `$doctype 5` を推奨しています。これは `<!DOCTYPE html>` を生成します。

```hamlet
$doctype 5
<html>
    <head>
        <title>Hamlet is Awesome
    <body>
        <p>All done.
```

<div class="yesod-book-notice">
これよりも古い構文として3つのエクスクラメーション (!!!) が今もサポートされています。時にはこのようなコードを見かけるかもしれません。この形式を削除する予定はありませんが、一般的に `doctype` による方法がより読みやすいと思います。
</div>

## Lucius 構文

Lucius はシェイクスピアに2種類存在する CSS テンプレート言語のうちの一つです。これは CSS のスーパーセットであることを意図しているため、既存の構文よりも機能が豊富です。

- Hamlet のように変数と URL の展開が可能です。
- CSS のブロックはネストが可能です。
- テンプレートで変数宣言が可能です。
- 複合的な CSS プロパティはミックスイン (mixin) として作り、複数の宣言で再利用することが可能です。

CSS ブロックのネストから説明を始めましょう。`article` 内部の複数のタグで特別なスタイルを適用させたいとします。普通の CSS では次のように書くでしょう。

```css
article code { background-color: grey; }
article p { text-indent: 2em; }
article a { text-decoration: none; }
article a > h1 { color: green; }
```

上記のコードは記述量は少ないですが article を何度も打たなければならないため多少不快ですし、もしそれが何十個もあったらどうでしょうか。世界一悪い事ではないにしても少し嫌です。Lucius はこのような場面で役立ちます。

```lucius
article {
    code { background-color: grey; }
    p { text-indent: 2em; }
    a { text-decoration: none; }
    > h1 { color: green; }
}
```

Lucius 変数を使えば繰り返しを避けることができます。単純な例として共通して使う文字色を定義してみましょう。

```lucius
@textcolor: #ccc; /* just because we hate our users */
body { color: #{textcolor} }
a:link, a:visited { color: #{textcolor} }
```

ミックスインは Lucius に比較的最近追加されたものです。アイデアとしてはプロパティの集合を与えるミックスインを宣言して、キャレット展開 (^) を使うことで、そのミックスインをテンプレートに埋め込みます。次の例はミックスインを利用してベンダープレフィックスを扱う方法を示すコードです。

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Text.Lucius
import qualified Data.Text.Lazy.IO as TLIO

-- Dummy render function.
render = undefined

-- Our mixin, which provides a number of vendor prefixes for transitions.
transition val =
    [luciusMixin|
        -webkit-transition: #{val};
        -moz-transition: #{val};
        -ms-transition: #{val};
        -o-transition: #{val};
        transition: #{val};
    |]

-- Our actual Lucius template, which uses the mixin.
myCSS =
    [lucius|
        .some-class {
            ^{transition "all 4s ease"}
        }
    |]

main = TLIO.putStrLn $ renderCss $ myCSS render
```

## Cassius 構文

Cassius と Lucius の違いは空白の扱いだけです。概要の部分で述べたように、処理エンジンは Lucius と同じですが、サブブロックを閉じるための波括弧や行末を表すセミコロンを挿入するための前処理を行います。これは Cassius を書く時に Lucius のすべての機能を使えるということを意味します。以下は簡単な例です。

```cassius
#banner
    border: 1px solid #{bannerColor}
    background-image: url(@{BannerImageR})
```

## Julius 構文

Julius は本章で論じられた言語の中で最も単純です。実際に単なる Javascript だという人もいるかもしれません。Julius ではこれまで述べた3種類の展開方法が利用できますが、それ以外については中身に何の変化も与えません。

<div class="yesod-book-notice">
Julius を scaffolded Yesod サイトで利用する場合 Javascript が自動的に圧縮されていることに気づくでしょう。これは Julius の機能ではなく Yesod が Julius の出力ファイルを圧縮するために hjsmin パッケージを利用しているためです。
</div>

## シェイクスピアの呼び出し方法

もちろんある点において疑問が生じるでしょう。実際にどのようにしてこれらのものを動かすのでしょうか？Haskell コードからシェイクスピアを呼び出すための方法は3種類あります。

#### 準クォート

準クォートを用いることで任意のコンテンツを Haskell コードに埋め込み、コンパイル時に Haskell コードへ変換することが可能となります。

#### 外部ファイル

外部ファイルの場合、テンプレートコードは別のファイルにあり Template Haskell を通して参照されます。

#### リロードモード

準クォートや外部ファイルを使った方法では、何か変更を加えると完全な再コンパイルが必要となります。リロードモードでは、テンプレートは別のファイルに保存されるため Template Haskell で参照します。しかし、実行時に外部ファイルは毎回最初から再パーズされます。

<div class="yesod-book-notice">
リロードモードは Hamlet では利用できません Cassius、 Lucius、 Julius でのみ利用可能です。Hamlet には Haskell コンパイラに直接依存し、実行時にうまく再実装できないようなあまりにも多くの洗練された側面があるからです。
</div>

プロダクション環境では、はじめの2つの方法の内どちらかを利用することになります。どちらともテンプレート全体を最終的な実行ファイルに埋め込むため、デプロイメントを簡潔にするとともにパフォーマンスを向上させます。準クォートの利点は全てが単一ファイルに存在するという簡潔さにあります。そのため、短いテンプレートであれば準クォートが適しています。しかし、一般的には次のような理由で外部ファイルによる方法が推薦されます。

- ロジックとデザインの分離という伝統にとても従っている。
- 簡単な CPP マクロを使うことで外部ファイルとデバッグモードを簡単に切り替えることができます。これにより、迅速な開発とプロダクション環境における高いパフォーマンスが達成可能になります。

これらは特別な準クォートと Template Haskell 関数であるため、適切な言語拡張を有効にして、正しい構文を使う必要があります。次のコードスニペットでそれぞれの簡単な例を見てみましょう。

### 準クォート

```haskell
{-# LANGUAGE OverloadedStrings #-} -- we're using Text below
{-# LANGUAGE QuasiQuotes #-}
import Text.Hamlet (HtmlUrl, hamlet)
import Data.Text (Text)
import Text.Blaze.Html.Renderer.String (renderHtml)

data MyRoute = Home | Time | Stylesheet

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = "/home"
render Time _ = "/time"
render Stylesheet _ = "/style.css"

template :: Text -> HtmlUrl MyRoute
template title = [hamlet|
$doctype 5
<html>
    <head>
        <title>#{title}
        <link rel=stylesheet href=@{Stylesheet}>
    <body>
        <h1>#{title}
|]

main :: IO ()
main = putStrLn $ renderHtml $ template "My Title" render
```

### 外部ファイル

```haskell
{-# LANGUAGE OverloadedStrings #-} -- we're using Text below
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-} -- to control production versus debug
import Text.Lucius (CssUrl, luciusFile, luciusFileReload, renderCss)
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TLIO

data MyRoute = Home | Time | Stylesheet

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = "/home"
render Time _ = "/time"
render Stylesheet _ = "/style.css"

template :: CssUrl MyRoute
#if PRODUCTION
template = $(luciusFile "template.lucius")
#else
template = $(luciusFileReload "template.lucius")
#endif

main :: IO ()
main = TLIO.putStrLn $ renderCss $ template render
```

```haskell
-- @template.lucius
foo { bar: baz }
```

これら関数の命名規則には次のような一貫性があります。

テンプレート言語 | 準クォート | 外部ファイル | リロード
-----------------|------------|--------------|----------
Hamlet  | `hamlet`  | `hamletFile`  | ***N/A***
Cassius | `cassius` | `cassiusFile` | `cassiusFileReload`
Lucius  | `lucius`  | `luciusFile`  | `luciusFileReload`
Julius  | `julius`  | `juliusFile`  | `juliusFileReload`

## その他の Hamlet 型

これまでのところ型安全URLが埋め込まれた HTML の断片のような Hamlet から `HtmlUrl` の値を生成する方法を見てきました。
その他にも Halmet が生成できる値は3つあります。単純な HTML、 URLと国際化されたメッセージを持った HTML、そしてウィジェットです。ウィジェットについてはウィジェットの章で詳しく説明します。

URL が埋め込まれていない単純な HTML を生成するために "シンプルな Hamlet" を使います。普通の Hamlet との違いがいくつかあります。

- 接頭辞に "s" が付く異なる種類の関数を利用します。したがって準クォートは `shamlet`、外部ファイルは `shamletFile` となります。これらをどのように発音するかについてはいまだ論争中です。
- URL 展開は許可されないため、コンパイル時エラーになります。
- 埋め込み (キャレット展開) は `HtmlUrl` の値を許可しなくなります。ルールとして埋め込み値はテンプレートそれ自体と同じ型を持つ必要があるので、この場合は `Html` 型でなければなりません。このことは `shamlet` において埋め込みは (ハッシュを用いた) 通常の変数展開で完全に置き換え可能であることを意味します。

Hamlet の国際化 (i18) への対応は少々複雑です。Hamlet はメッセージデータ型で i18n をサポートします。それは概念・実装面において型安全URLとかなり類似しています。例として hello と言った後にりんごをいくつ購入したかを述べるアプリケーションを作りたいとしましょう。これらのメッセージをデータ型を使って以下のように表現します。

```haskell
data Msg = Hello | Apples Int
```

次に、これを人間が読める形式に変換したいので、レンダリング関数をいくつか定義します。

```haskell
renderEnglish :: Msg -> Text
renderEnglish Hello = "Hello"
renderEnglish (Apples 0) = "You did not buy any apples."
renderEnglish (Apples 1) = "You bought 1 apple."
renderEnglish (Apples i) = T.concat ["You bought ", T.pack $ show i, " apples."]
```

そして Msg の値を直接テンプレート内で展開するためにアンダースコア展開を使います。

```hamlet
$doctype 5
<html>
    <head>
        <title>i18n
    <body>
        <h1>_{Hello}
        <p>_{Apples count}
```

この種のテンプレートは、型安全URLをレンダリング関数に渡した時のように、これらの値を HTML に変換する何らかの方法を必要とします。

これを表現するために、新しい型シノニムを定義します。

```haskell
type Render url = url -> [(Text, Text)] -> Text
type Translate msg = msg -> Html
type HtmlUrlI18n msg url = Translate msg -> Render url -> Html
```

これによって `renderEnglish`、 `renderSpanish`、 `renderKlingon` をテンプレートに渡すことができ、それはうまく翻訳された出力を生成します (もちろん翻訳機の性能に依存します・・・)。
完全なプログラムは以下のようになります。

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as T
import Text.Hamlet (HtmlUrlI18n, ihamlet)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)

data MyRoute = Home | Time | Stylesheet

renderUrl :: MyRoute -> [(Text, Text)] -> Text
renderUrl Home _ = "/home"
renderUrl Time _ = "/time"
renderUrl Stylesheet _ = "/style.css"

data Msg = Hello | Apples Int

renderEnglish :: Msg -> Text
renderEnglish Hello = "Hello"
renderEnglish (Apples 0) = "You did not buy any apples."
renderEnglish (Apples 1) = "You bought 1 apple."
renderEnglish (Apples i) = T.concat ["You bought ", T.pack $ show i, " apples."]

template :: Int -> HtmlUrlI18n Msg MyRoute
template count = [ihamlet|
$doctype 5
<html>
    <head>
        <title>i18n
    <body>
        <h1>_{Hello}
        <p>_{Apples count}
|]

main :: IO ()
main = putStrLn $ renderHtml
     $ (template 5) (toHtml . renderEnglish) renderUrl
```

## その他のシェイクスピア

HTML、CSS、Javascript の補助だけでなく、より一般的な利用用途がシェイクスピアには存在します。シェイクスピアのテキストは Ruby や Python のようにスクリプト言語で慣れているような展開された文字列を作るための簡単な方法を提供します。そのため、明らかにこのパッケージの利用用途は Yesod に限定されません。

```haskell
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
import Text.Shakespeare.Text
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text (Text)
import Control.Monad (forM_)

data Item = Item
    { itemName :: Text
    , itemQty :: Int
    }

items :: [Item]
items =
    [ Item "apples" 5
    , Item "bananas" 10
    ]

main :: IO ()
main = forM_ items $ \item -> TLIO.putStrLn
    [lt|You have #{show $ itemQty item} #{itemName item}.|]
```

この単純な例におけるポイントを以下に挙げます。

- 文字列を表すデータ型には `String`, 正格 `Text`, 遅延 `Text` があることに注意してください。それらは共に全てうまく機能します。
- 遅延テキストを生成したい場合は `lt` という名称の準クォートを使います。同様に `st` は正格テキストを生成します。
- これらの準クォートには長い名称も存在します。 (`ltext` と `stext`)

## 一般的推奨事項

これは Yesod コミュニティからシェイクスピアを最大限に利用するための一般的なヒントです。

- 実際のサイトにおいては外部ファイルを利用してください。ライブラリにおいては長くなり過ぎない限り、準クォートを利用しても良いです。
- Patrick Briston 氏が非常に有益な [Vim code hilighter](https://github.com/pbrisbin/html-template-syntax) をまとめあげました。
- Hamlet タグは既存のタグの後に開始・終了タグを埋め込むのではなく、常に新たな行から始める必要があります。これに関する唯一の例外はまれに大きなテキストブロックにおいて出現する `<i>`、 `<b>` タグだけです。

## 本書のコード

- [Example01.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch04/Example01.hs)
- [Example02.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch04/Example02.hs)
- [Example03.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch04/Example03.hs)
- [Example04.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch04/Example04.hs)
- [Example05.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch04/Example05.hs)
- [Example06.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch04/Example06.hs)
- [Example07.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch04/Example07.hs)
- [Example08.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch04/Example08.hs)
