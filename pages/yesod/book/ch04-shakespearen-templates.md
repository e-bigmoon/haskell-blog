---
title: Shakespearen Templates
date: 2018/03/18
---

## Shakespearen Templates

Yesod では一般的にテンプレート言語のシェイクスピアファミリーを利用して HTML、CSS、Javascript を生成します。
このテンプレート言語は以下のような包括的原則とともに共通の構文を持ちます。

- ベースの言語 (Html, Css, Javascript) に対する干渉をできる限り少なくし、控えめに便利機能を提供します
- 妥当なコンテンツ (well-formed content) であることをコンパイル時に保証します
- 静的型安全性は [XSS攻撃(cross-site scripting)](http://en.wikipedia.org/wiki/Cross-site_scripting) の防止に非常に役立ちます
- 型安全URLによって。可能な場合は常に展開されたリンクを自動的に検証します

Yesod とこれらのテンプレート言語の間には本質的な繋がりやその他の制限は何もありません。
そのため、それぞれは互いに独立して用いることができます。
本章の前半で、これらテンプレート言語自体について説明し、後半ではそれらを Yesod アプリケーション開発のために利用する方法を述べる。

## Synopsis

シェイクスピアには4つの主要な言語があり、Hamlet は HTML、Julius は Javascript、Cassius と Lucius は両者とも CSS のためのテンプレート言語です。
Hamlet と Cassius は空白によって識別される形式であり、インデントを用いてネストを表現します。
それに対して Lucius は CSS のスーパーセットなので、通常の CSS で用いられる波括弧の形式でネストを表します。
Julius は Javascript を作り出すための単なる透過型言語であり、唯一追加された機能は変数展開 (variable interpolation) だけです。

Cassius は単に Lucius の代替構文です。
内部では両者とも同じ処理エンジンを用いていますが、 Cassius のファイルは前処理の段階ででインデントを括弧に変換しています。
どちらを利用するかは純粋に構文の好みとなります。

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

### Julius(Javascript)

```julius
$(function(){
    $("section.#{sectionClass}").hide();
    $("#mybutton").click(function(){document.location = "@{SomeRouteR}";});
    ^{addBling}
});
```

## 型

構文に入る前に関連する型をいろいろ見てみましょう。
導入部において型は XSS 攻撃の防止になると述べました。
例えば、誰かの名前を表示する次のような HTML テンプレートがあったとしましょう。

```hamlet
<p>Hello, my name is #{name}
```

`#{...}` はシェイクスピアでは、変数展開となります。

`name` では何が起こり、さらにそのデータ型は何であるべきでしょう？
よくある方法として `Text` の値をそのまま挿入することです。
しかし、そのようにすると `name` が次のような場合に非常に問題となります。

```html
<script src='http://nefarious.com/evil.js'></script>
```

このとき `<` が `&lt;` になるように `name` をエンティティエンコードしたいのですが、どうすれば良いでしょうか。

その場合のよくある方法は単に埋め込まれる全てのテキストをエンティティエンコードすることです。
もし、別の処理によって既に HTML が生成されていたとしたら何が起こるでしょうか？
例えば、この Yesod のウェブサイトではすべての Hakell コードスニペットは適切な `span` タグで覆われることで色付けされています。
仮に全てをエンティティエスケープしたとすれば、コードスニペットはほとんど読めない状態となるでしょう。

そうならないように `Html` データ型を使います。2種類のAPIによって `Html` 型の値を生成する.
`ToMarkup` 型クラスは `toHtml` 関数によって `String` と `Text` の値を `Html` の値に変換する方法を提供し、自動的に途中でエンティティエスケープを行います。

これは先ほどの `name` で利用したい方法です。
また、コードスニペットの例においては `preEscapedToMarkup` を利用するでしょう。

Hamlet (HTML シェイクスピア言語) の変数展開では、値に対し自動的に `toHtml` が適用されます。
したがって `String` を展開する場合はエンティティエスケープされますが、 `Html` の場合は何もしません。
コードスニペットの例においては `#{preEscapedToMarkup myHaskellHtml}` のようにして展開を行うでしょう。

`Html` データ型は上に述べられた関数と共に blaze-html パッケージにより提供されます。
これによって Hamlet が他の blaze-html パッケージと相互作用することが可能になり、 blaze-html 値を生成するための一般的解決策ともなります。
さらには blaze-html の素晴らしいパフォーマンスの恩恵を得ることもできます。

同様に `CSS型 と ToCSS クラス`, `Javascript型 と ToJavascript クラス` が存在します。
これらは CSS で偶然 HTML を落とさないようにするためにコンパイル時におけるサニティーチェックを行います。

CSS 側における他の利点としては、色や単位に関するデータ型があることです。

```lucius
.red { color: #{colorRed} }
```

さらなる詳細については Haddock文書を参照してください。

## Type-safe URLs

おそらく Yesod における最も独特な機能は型安全URLであり、シェイクスピアにおいて扱うことができます。
使用法は変数展開とほとんど同じ、ハッシュ (#) の代わりにアットマーク (@) にするだけです。
構文は後で確認することにして、まずは直感的に理解しましょう。

まず、次のような2つのルートを持つアプリケーションがあるとしましょう。
`http://example.com/profile/home` はトップページで `http://example.com/display/time` は現在の時間を表示します。
ここで、トップページから time ページにリンクしたいとき、 URL を構築するためには3つの異なる方法が考えられます。

1. 相対リンク: ***../display/time***
1. ドメイン無しの絶対リンク: ***/display/time***
1. ドメイン有りの絶対リンク: `http://example.com/display/time`

それぞれの方法において問題があります。
1つ目の方法はどちらかのURLが変化することでリンク切れになってしまいますし、全ての状況に適しているというわけではありません。
例えば RSS や Atom フィードには絶対URLが必要となります。
2つ目は1つ目の方法よりも変化に対し柔軟ではありますが、 RSS や Atom の問題は依然として存在する。
3つ目は全ての場合に有効ですが、ドメイン名が変わるごとに全ての URL を更新する必要が生じます。
そのようなことはそう頻繁には起こらないだろうと思うかもしれませんが、開発からステージングそして最終の本番サーバへの移行を考えてみてください。

しかしより重要なことはそれぞれの方法において、ひとつ非常に大きな問題があるということです。
それは、仮にルートを変更したとしてもコンパイラは壊れたリンクに対して警告をしないという点です。
誤植が大混乱を招くように、これは最悪な状況だということは言うまでもありません。

型安全URLの目的は人間の代わりにコンパイラにできる限り多くのチェックを行わせることです。
これを可能にするための最初のステップとしては、コンパイラが理解できない単純な文字列から離れ、正しく定義されたデータ型に変更することです。
今回の単純なアプリケーションではルートを直和型で表現します。

```haskell
data MyRoute = Home | Time
```

テンプレートに /display/time のようなリンクを記述する代わりに `Time` コンストラクタを利用します。
しかし、そうすると HTML はデータ型ではなくテキストでできているため、これらの値をテキストに変換する方法が必要となります。
その変換を行う関数を URLレンダリング関数 (URL rendering function) と呼び、簡単な例を以下に示します。

```haskell
renderMyRoute :: MyRoute -> Text
renderMyRoute Home = "http://example.com/profile/home"
renderMyRoute Time = "http://example.com/display/time"
```

URLレンダリング関数は実際にはこれより少しだけ複雑です。
それらはクエリ文字列パラメータを追加し、コンストラクタのレコードを扱い、より高度にドメイン名を処理する必要があります。しかし、 Yesod は自動的にレンダ関数を生成するため実際にはこれらについて考える必要はありません。
唯一指摘すべきこととしてはクエリ文字列を扱うために、型シグネチャが実際にはもう少し複雑であることです。

```haskell
type Query = [(Text, Text)]
type Render url = url -> Query -> Text
renderMyRoute :: Render MyRoute
renderMyRoute Home _ = ...
renderMyRoute Time _ = ...
```

こうしてレンダ関数が定義され、テンプレートに埋め込まれた型安全URLを使えるようになりました。
これは正確にはどのようにして上手く動作するのでしょうか？
`Html` (`CSS` や `Javascript`) 値を直接生成する代わりにテンプレート言語は関数を生成し、その関数はレンダ関数を取り HTML を生成します。
このことをより詳細に見るために Hamlet が表面下においてどのように動くかを軽く覗いてみましょう。
以下のテンプレートがあると仮定します。

```hamlet
<a href=@{Time}>The time
```

これは、だいたい次のような Haskell コードに翻訳されます。

```haskell
\render -> mconcat ["<a href='", render Time, "'>The time</a>"]
```

## Syntax

あらゆるシェイクスピアのテンプレート言語は同じ展開構文を備えているため、型安全URLを用いることができます。
それらは対象となる言語 (HTML、CSS、Javascript) 特有の構文とは異なります。
それぞれの言語を順番に見てみましょう。

## Hamlet Syntax

Hamlet はシェイクスピアの中でも最も洗練されたものです。
それは HTML 生成のための構文を提供するだけでなく、条件、ループ、maybe などの基本的な制御構造を備えます。

### Tags

タグがあらゆる HTML テンプレート言語において重要な役割を担うことは明らかです。
Hamlet においては、テンプレート言語を使いやすくするためにできる限り既存の HTML 構文に似せているが、ネスとの表現は閉じタグの代わりにインデントを利用する。
したがって、次のようなHTMLコードは

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
このような Hamlet コードに変換される.

```hamlet
<body>
    <p>Some paragraph.
    <ul>
        <li>Item 1
        <li>Item 2
```

一般的に、この表記に一度慣れてしまえば通常のHTMLより解読がしやすくなると思います。
唯一の技巧的な部分はタグ前後の空白の扱いです。
例えば、次のようなHTMLを生成したいとしましょう。

```html
<p>Paragraph <i>italic</i> end.</p>
```

空白は "Paragraph" の後の部分や "end" の前の部分で保持されていて欲しいです。
そのために2つの単純なエスケープ文字を用います。

```hamlet
<p>
    Paragraph #
    <i>italic
    \ end.
```

空白のエスケープ規則は実にシンプルです。

1. もし、行頭の文字が空白でなく、バックスラッシュであれば、そのバックスラッシュは無視される。(注意: これは、その行における全てのタグが単純なテキストとして処理されてしまう。)
2. もし、行末の文字がハッシュの場合それは無視されます

もう一つ別のこととして Hamlet はコンテンツ内のエンティティをエスケープしません。
これによって既存の HTML のコピーが容易になります。
したがって。上の例は次のように書くこともできます。

```hamlet
<p>Paragraph <i>italic</i> end.
```

Hamlet では開始タグは自動的に閉じられることに注意してください。
一方で内側の "i" タグはそうではありません。
どちらの方法もペナルティ無く自由に使うことができるため、好きな方を選択できます。
しかし Hamlet において閉じタグを用いる唯一の場面は、このようなインラインタグに限ることに注意してください。
インラインタグ以外で閉じタグを用いることはありません。

これによる他の結果としては、最初のタグより後に出現するタグはIDやクラスに関する特別な処理を行わないということです。
例えば、次の Hamlet スニペットは次のような HTML コードを生成します。

```hamlet
<p #firstid>Paragraph <i #secondid>italic</i> end.
```

生成された HTML コード。

```html
<p id="firstid">Paragraph <i #secondid>italic</i> end.</p>
```

`i` タグが単純なテキストとして処理される一方で `p`タグが自動的に閉じられ、その属性が特別扱いを受ける点に注意してください。

### Interpolation

ここまで出てきたものは簡潔な HTML であるため何の問題も起こっていませんが、まだ Haskell コードと相互作用することはできていません。
どのようにして変数を受け渡すのでしょうか？
それは単純に展開を利用します。

```hamlet
<head>
    <title>#{title}
```

ハッシュの後ろに波括弧のペアが続く場合は変数展開 (variable interpolation) です。
上の例においてはテンプレートが呼ばれたスコープで `title` 変数が用いられます。
そのことを次のように言い直せます。
Hamlet は変数が呼ばれた際に自動的にスコープ内の変数へアクセスし、特に変数を受け渡す必要はありません。

展開の中では関数適用、文字列、数値リテラル、修飾付きモジュールを利用することができます。
また、処理をまとめるために丸括弧やドルマークを用いることができます。
展開の処理の最後に `toHtml` 関数が適用されます。
これは `ToHtml` クラスの全てのインスタンスが展開可能だということを意味します。
例えば、次のコードを考えてみましょう。

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

目玉機能の型安全URLについてはどうでしょうか？
それらはシャープの代わりにアットマーク (@) で始まる点を除いて、あらゆる面で変数展開と同一です。
さらにキャレット (^) によって同じ型の他のテンプレートを埋め込むことが可能になります。
次のコード例で確認しましょう。

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

さらに、クエリ文字列パラメータを受け入れるような URL 展開の変種があり、これは例えばページ番号をつけるようなレスポンスに適しています。
`@{...}` を用いる代わりにクエスチョンマークを加え `@?{...}` とすることで、クエリ文字列の存在を示します。
与える値は2要素のタプルである必要があり、第1要素が型安全URL、第2要素はクエリ文字列パラメータのリストのペアとなります。
例えば、次のコードスニペットの例を見てみましょう。

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

### Attributes

前の最後の例では "a" タグに href 属性を付加しました。
構文について詳細に見てみましょう。

- 属性の値にも変数展開記法が利用できます
- HTML と同様に属性のイコール記号と値はオプションです。したがって `<input type=checkbox checked>` は完全に妥当です
- id と class 属性についてはそれぞれハッシュ (#) と ピリオド (.) を用います。つまり `<p #paragraphid .class1 .class2>` という感じです
- 属性値の前後のクォートは任意ですが、途中でスペースを用いる場合は必要です
- コロンを利用することでオプションの属性を追加できます。isChecked の値が True の場合のみチェックボックスをチェックした状態にしたい場合 `<input type=checkbox :isChecked:checked>` とします。また、ある段落の文字色を赤色にしたい場合は `<p :isRed:style="color:red">` とします。(これはクラス名を用いても良いです。例えば、 `<p :isCurrent:.current>` は isCurrent が `True` の場合, `current` クラスをセットします)
- 任意のキーと値のペアもまた `*{...}` 構文を用いて展開が可能です。属性の場合の変数展開は値が String または Text のタプルか, タプルのリストである必要があります。例えば `attrs = [("foo", "bar")]` という値であれば展開可能です。

### Conditionals

いつかはページ内に何らかのロジックを組み込みたいと思うでしょう。
Hamlet の目的はロジックを可能な限り最小にすることで Haskell に大部分を任せることです。
そのようなものとして `if`、 `ifelse`、 `else` は非常に基本的な論理文 (logical statements) です。

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

同様に Maybe 値を扱うための特別な記法があります。
技術的には `if`、 `isJust`、 `fromJust` を用いることで処理されますが、部分関数は避けることができるため、よく使われます。

```hamlet
$maybe name <- maybeName
    <p>Your name is #{name}
$nothing
    <p>I don't know your name.
```

単純な識別子に加え、コンストラクタやタプルなど、より複雑な値を左辺に用いることができます。

```hamlet
$maybe Person firstName lastName <- maybePerson
    <p>Your name is #{firstName} #{lastName}
```

右辺は展開と同じ規則に従い、変数や関数適用などが使えます。

### Forall

リストのループについてはどうでしょうか？
以下のようにするだけです。

```hamlet
$if null people
    <p>No people.
$else
    <ul>
        $forall person <- people
            <li>#{person}
```

### Case

パターンマッチは Haskell の重要な強みの1つです。
直和型は多くの実務的な型をきれいにモデル化することができ `case` 文を利用すると安全にマッチさせることができます。
つまりどのケースにも当てはまらない場合にコンパイルは警告を出します。
Hamlet の case 文も同じように働きます。

```hamlet
$case foo
    $of Left bar
        <p>It was left: #{bar}
    $of Right baz
        <p>It was right: #{baz}
```

### With

文を短くするために `with` を利用します。
基本的には長い式に対して、単なる利便性のためのエイリアス宣言となります。

```hamlet
$with foo <- some very (long ugly) expression that $ should only $ happen once
    <p>But I'm going to use #{foo} multiple times. #{foo}
```

### Doctype

最後はちょっとした糖衣構文の doctype 文です。
`dectype` は複数の異なるバージョンがサポートされていますが、現代のウェブアプリケーションにおいては `$doctype 5` が推奨され、これは `<!DOCTYPE html>` を生成します。

```hamlet
$doctype 5
<html>
    <head>
        <title>Hamlet is Awesome
    <body>
        <p>All done.
```

まだサポートされているこれよりも古い構文として3つのエクスクラメーション (!!!) があり、時にはこのようなコードを見かけるかもしれません。
この形式を削除する予定はありませんが、一般的に `doctype` による方法がより読みやすいと思います。

## Lucius Syntax

Lucius はシェイクスピアに2種類存在する CSS テンプレート言語のうちの一つです。
これは CSS のスーパーセットであることを意図しているため、既存の構文よりも機能が豊富です。

- Hamlet のように変数と URL の展開が可能です
- CSS のブロックはネストが可能です
- テンプレートで変数宣言が可能です
- 複合的な CSS プロパティはミックスイン (mixin) として作り、複数の宣言で再利用することが可能です

CSS ブロックのネストから説明を始めましょう。
`article` 内部の複数のタグで特別なスタイルを適用させたいとします。
普通の CSS では次のように書くでしょう。

```css
article code { background-color: grey; }
article p { text-indent: 2em; }
article a { text-decoration: none; }
article a > h1 { color: green; }
```

上記のコードは記述量は少ないですが article を何度も打たなければならないため多少不快ですし、もしそれが何十個もあったらどうでしょうか。
世界一悪い事ではないにしても少し嫌です。
Lucius はこのような場面で役立ちます。

```lucius
article {
    code { background-color: grey; }
    p { text-indent: 2em; }
    a { text-decoration: none; }
    > h1 { color: green; }
}
```

Lucius 変数を使えば繰り返しを避けることができます。
単純な例として共通の文字色を定義してみましょう。

```lucius
@textcolor: #ccc; /* just because we hate our users */
body { color: #{textcolor} }
a:link, a:visited { color: #{textcolor} }
```

ミックスインは Lucius において比較的新しく追加されたものです。
アイデアとしてはプロパティの集合を与える mixin を宣言して、キャレット展開(^)を用いることで、そのミックスインをテンプレートに埋め込みます。
次の例はミックスインを利用してベンダープレフィックスを扱う方法を示すコードです。

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

## Cassius Syntax

Cassius と Lucius の違いは空白の扱いだけです。
概要の部分で述べたように、これは Lucius と同じ処理エンジンを用いていますが、サブブロックを閉じるための波括弧や行末を表すセミコロンを挿入するための前処理を行います。
これは Cassius を実行する際に Lucius のすべての機能を用いることができることを意味します。
以下は素朴な例です。

```cassius
#banner
    border: 1px solid #{bannerColor}
    background-image: url(@{BannerImageR})
```

## Julius Syntax

Julius は本章で論じられた言語の中で最も単純です。
実際に単なる Javascript であるという人もいるかもしれません。
Julius ではこれまで述べた3種類の展開方法が利用可能ですが、それ以外については中身に何の変化も与えません。

Julius を scaffolded Yesod サイトで用いる場合 Javascript が自動的に圧縮されていることに気づくでしょう。
これは Julius の機能ではなく Yesod が Julius の出力ファイルを圧縮するために hjsmin パッケージを用いているためです。

## Calling Shakespeare

もちろんある点において疑問が生じるでしょう。
実際にどのようにしてこれらのものを動かすのでしょうか？
Haskell コードからシェイクスピアを呼び出す3つの異なる方法が存在します。

### Quasiquotes

準クォートを用いることで任意のコンテンツを Haskell コードに埋め込み、コンパイル時に Haskell コードへ変換することが可能となります。

### External file

外部ファイルの場合、テンプレートコードは別のファイルにあり Template Haskell を通して参照されます。

### Reload mode

上の両者は何か変更を加えると完全な再コンパイルが必要となります。
リロードモードにおいてはテンプレートは別のファイルにあり Template Haskell を用いて参照します。
しかし、実行時に外部ファイルは毎回最初から再パーズされます。

リロードモードは Hamlet では用いることができず Cassius、 Lucius、 Julius のみ利用可能です。
Hamlet には Haskell コンパイラに直接依存し、実行時にうまく再実装できないようなあまりにも多くの洗練された側面があるからです。

本番環境では、はじめの2つの方法の内どちらかを用いることが可能です。
両者共に、テンプレート全体を最終的な実行ファイルに埋め込み、デプロイメントを簡易にすることでパフォーマンスを向上させます。
準クォートの利点は全てが単一ファイルに存在するという簡潔さにあります。そのため、短いテンプレートであれば準クォートが適しています。
しかし、一般的には次のような理由で外部ファイルによる方法が推薦されます。

- ロジックとデザインの分離という伝統にとても従っている
- 簡単なCPPマクロを用いて外部ファイルとデバッグモードを容易に変更可能です。これにより、迅速な開発と本番環境における高いパフォーマンスが達成可能になる。

これらは特別な準クォートと Template Haskell 関数であるため、適切な言語拡張を有効にし正しい構文を用いる必要があります。
次のコードスニペットにおいてそれぞれの簡単な例を見て見ましょう。

## Quasiquoter

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

## External file

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
Julius  | `julius`  | `JuliusFile`  | `JuliusFileReload`

## Alternate Hamlet Types

これまでのところ型安全URLが埋め込まれた HTML の断片のような Hamlet から `HtmlUrl` の値を生成する方法を見てきました。
その他にも Halmet を用いて生成できる値が3つあります。
単純な HTML、 URLと国際化されたメッセージを持ったHTML、 そしてウィジェットです。
ウィジェットについてはウィジェットの章において詳細を説明します。

URLの埋め込まれていない単純な HTML を生成するために "純粋なHamlet" (simplified Hamlet) を用います。
ここにはいくつかの変更点があります。

- 接頭辞に "s" が付く異なる種類の関数を利用します。したがって準クォートは `shamlet`、外部ファイルは `shamletFile` となります。これらをどのように発音するかについてはいまだ論争中です
- URL 展開は許可されないため、コンパイル時エラーとなります
- 埋め込み (キャレット展開) はもはや `HtmlUrl` 値を許可しません。ルールとして埋め込み値はテンプレートそれ自体と同じ型を持つ必要があるため, この場合は `Html` でなければなりません。このことは `shamlet` において埋め込みは (ハッシュを用いた) 通常の変数展開で完全に置き換え可能であることを意味します。

Hamlet の国際化 (i18) に対処することは多少複雑です。
Hamlet はメッセージデータ型により i18n をサポートします。
それは概念・実装面において型安全URLとかなり類似しています。
動機付けのための例として hello と言った後にいくつのりんごを食べたかを述べるアプリケーションを作りたいとしましょう。
これらのメッセージをデータ型を用いて以下のように表現します。

```haskell
data Msg = Hello | Apples Int
```

次に、それを人間が読める形式に変換したいので、レンダ関数をいくつか定義します。

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

この種のテンプレートは、型安全URLをレンダー関数を渡した時のように、これらの値を HTML に変換する何らかの方法を必要とします。

これを表現するために、新しい型シノニムを定義します。

```haskell
type Render url = url -> [(Text, Text)] -> Text
type Translate msg = msg -> Html
type HtmlUrlI18n msg url = Translate msg -> Render url -> Html
```

これによって `renderEnglish`、 `renderSpanish`、 `renderKlingon` をテンプレートに渡すことができ、それはうまく翻訳された出力を生成します (もちろん翻訳機の性質に依存します・・・)。
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

## Other Shakespeare

HTML、 CSS、 Javascript の補助だけでなく、より一般的な利用用途がシェイクスピアには存在します。
シェイクスピアのテキストは Ruby や Python のようにスクリプト言語で慣れているような展開された文字列を作るための簡単な方法を提供します。
そのため、明らかにこのパッケージの利用用途は Yesod に限定されません。

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

- 文字列を表すデータ型には `String`, 正格 `Text`, 遅延 `Text` があることに注意してください。それらは共に全てうまく機能します
- 遅延テキストを生成したい場合は `lt` という名称の準クォートを使います。 `st` の場合も同様です
- これらの準クォートには長い名称も存在します (`ltext` と `stext`)

## General Recommendations

これは Yesod コミュニティからシェイクスピアを最大限に利用するための一般的なヒントです。

- 実際のサイトにおいては外部ファイルを利用してください。ライブラリにおいては長くなり過ぎない限り、準クォートを利用しても良いです
- Patrick Briston 氏が非常に有益な [Vim code hilighter](https://github.com/pbrisbin/html-template-syntax) をまとめあげました
- Hamlet タグは既存のタグの後に開始・終了タグを埋め込むのではなく、常に新たな行から始める必要があります。これに関する唯一の例外はまれに大きなテキストブロックにおいて出現する `<i>`、 `<b>` タグだけです