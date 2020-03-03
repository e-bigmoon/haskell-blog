---
title: 国際化
published: 2019/04/29
# updated: 2020/02/02
---

ユーザはソフトウェアが自国の言語に対応していることに期待しています。私たちにとっては都合が悪いことに、1つ以上の言語が関連しています。単に文字列を置換するだけであれば、それほど複雑ではありませんが、あらゆる文法の問題を適切に扱うことは難しい場合があります。結局のところ、プログラムの出力から 'List 1 file(s)' などという結果を誰が見たがるのでしょうか?

しかし、本当の i18n による解決策は、単に正しい出力を得るための方法を与える以上のことを必要とします。それは、この過程をプログラマと翻訳者にとって簡単で、できるだけエラーのないものにする必要があります。この問題に対する Yesod の答えは以下の通りです。

- リクエストヘッダに応じてユーザの求める言語を賢く推測し、上書きできること
- Haskell の知識を必要としない、翻訳のための単純な構文であること (要するに、多くの翻訳者はプログラマではありません)
- 大部分の必要性をカバーするデフォルトの補助関数の選択とともに, 必要に応じて複雑な文法の問題に対しては、Haskell の最大限の力を利用可能なこと
- 語順は全く問題ありません

## 概要

```text
-- @messages/en.msg
Hello: Hello
EnterItemCount: I would like to buy:
Purchase: Purchase
ItemCount count@Int: You have purchased #{showInt count} #{plural count "item" "items"}.
SwitchLanguage: Switch language to:
Switch: Switch
```

```text
-- @messages/he.msg
Hello: שלום
EnterItemCount: אני רוצה לקנות:
Purchase: קנה
ItemCount count: קנית #{showInt count} #{plural count "דבר" "דברים"}.
SwitchLanguage: החלף שפה ל:
Switch: החלף
```

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data App = App

mkMessage "App" "messages" "en"

plural :: Int -> String -> String -> String
plural 1 x _ = x
plural _ _ y = y

showInt :: Int -> String
showInt = show

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "App" [parseRoutes|
/     HomeR GET
/buy  BuyR  GET
/lang LangR POST
|]

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <h1>_{MsgHello}
        <form action=@{BuyR}>
            _{MsgEnterItemCount}
            <input type=text name=count>
            <input type=submit value=_{MsgPurchase}>
        <form action=@{LangR} method=post>
            _{MsgSwitchLanguage}
            <select name=lang>
                <option value=en>English
                <option value=he>Hebrew
            <input type=submit value=_{MsgSwitch}>
    |]

getBuyR :: Handler Html
getBuyR = do
    count <- runInputGet $ ireq intField "count"
    defaultLayout [whamlet|<p>_{MsgItemCount count}|]

postLangR :: Handler ()
postLangR = do
    lang <- runInputPost $ ireq textField "lang"
    setLanguage lang
    redirect HomeR

main :: IO ()
main = warp 3000 App
```

## 概観

既存の良くある i18n の解決策は、gettext や Java message bundle のように文字列検索の原理で機能します。良くあるのは、変数を文字列に挿入する printf の展開です。ご想像の通り、Yesod においては代わりに型に頼っています。これはコンパイラが自動的に誤りを見つけてくれるような、あらゆる標準的な利点があります。

具体例を取り上げてみましょう。アプリケーションがユーザに言いたいことが2つあるとしましょう。それは hello と言うことと、何人のユーザはシステムにログインしているかということです。これは直和型でモデル化できます。

```haskkell
data MyMessage = MsgHello | MsgUsersLoggedIn Int
```

このデータ型を英語表現に変換するための関数も書くこともできます。

```haskell
toEnglish :: MyMessage -> String
toEnglish MsgHello = "Hello there!"
toEnglish (MsgUsersLoggedIn 1) = "There is 1 user logged in."
toEnglish (MsgUsersLoggedIn i) = "There are " ++ show i ++ " users logged in."
```

ほかの言語についても同様な関数を書くことができます。この内部 Haskell 的な方法による利点は、特に複数性のような、トリッキーな文法的問題を解決するために Haskell の力を最大限に利用できることです。

<div class=yesod-book-notice>
1つのアイテムに関しては1つの方法があり、他の数に関しては、他の方法があると言う意味で、複数性はそれほど複雑ではないと思うかもしれません。それは英語については本当かもしれませんが、すべての言語について当てはまるわけではありません。例えば、ロシア語は6つの異なる形式があり、どれを使うかについては modulus logic で決める必要があります。
</div>

しかし、この方法の欠点はこれらすべてを Haskell の内部に書かなくてはならないので、翻訳者に対して優しくありません。これを解決するために Yesod はメッセージファイルの概念を導入しました。あとで詳しく紹介します。

これら一式の翻訳関数を持っていると仮定した場合、それらをどのように使えば良いのでしょうか？必要なものとしては、翻訳関数一式をすべてをまとめあげ、ユーザの選択した言語に応じて適切な翻訳関数を選択する新しい関数です。その関数さえあれば、Yesod は自動的にもっとも関連するレンダリング関数を選び、与えた値に基づいてそれを呼び出すことができます。

多少物事を単純化するために Hamlet は特別な展開構文 `_{...}` を持ちます。この構文はレンダリング関数に対するあらゆる呼び出しを扱います。そして、レンダリング関数をアプリケーションに関連付けるために `YesodMessage` 型クラスを使います。

## メッセージファイル

メッセージファイルを使えば、とてもシンプルに翻訳を作ることができます。設定は簡単です。それぞれの言語で、すべての翻訳を含む1つのファイルを用意し、そしてそれらの翻訳ファイル全てを1つのフォルダに含めます。各ファイル名は *en.msg* のように、言語コードに基づいた名前になります。そして、ファイルの各行は1つのフレーズを処理し、それはメッセージデータ型の1つのコンストラクタに対応します。

<div class=yesod-book-notice>
scaffolded サイトはすでに完成したメッセージフォルダを含みます。
</div>

まず初めに、言語コードについて説明します。言語コードの指定方法には「2文字の言語コード」と「言語-地域コード」の2種類の選択肢があります。例えば、ブラウザがページを読み込む時に en-US と en の2つの言語コードを送信するとしましょう。その場合、私のブラウザは "もしアメリカ英語があれば、それが一番好ましいです。次に、もし英語があれば代わりにそれを利用します。" というように言っています。

では、あなたのアプリケーションはどちらの形式を利用すべきでしょうか？実際に地域毎に異なる翻訳を作っている場合を除いて、基本的に2文字のコードを利用します。こうすれば、カナダ英語を求めている人でも英語を見ることができます。このような場面において、Yesod は関連する2文字のコードを追加するでしょう。例えば、ユーザの言語リストが以下のようになっていたとしましょう。

```
pt-BR, es, he
```

これは "ブラジル系ポルトガル語が好きです。そして、次に好きなのはスペイン語、ヘブライ語です。" という意味です。また、あなたのアプリケーションは pt(一般的なポルトガル語) と英語 (英語はデフォルト) を提供するとしましょう。ユーザの言語リストに厳密に従えば、ユーザは英語を見ることになります。そのため Yesod では代わりに、その言語リストを次のように変換します。

```
pt-BR, es, he, pt
```

つまり、地域毎に異なる翻訳を提供しないのであれば、2文字の言語コードを利用します。

それでは、メッセージファイルについてはどうでしょうか？Hamlet や Persistent に取り組んだ後であれば、構文は非常に馴染み深いものでしょう。行はメッセージ名から始まります。これはデータコンストラクタなので、大文字で始まる必要があります。次に、個々のパラメータを持つことができ、それらは小文字で始まります。これはデータコンストラクタの引数になります。引数リストはコロンで終了し、そして、翻訳される文字列が続きます。ここでは、複数性などのような問題を扱うために、典型的な変数展開構文の翻訳補助関数を利用でき、すべての必要な翻訳されたメッセージを作ることができます。

### 型を指定する

データ型はメッセージの仕様により作られるため、データコンストラクタの各パラメータにデータ型を指定する必要があります。このために @構文を使います。例えば、`data MyMessage = MsgHello | MsgSayAge Int` のデータ型を作るためには、以下のように記述します。

```haskell
Hello: Hi there!
SayAge age@Int: Your age is: #{show age}
```

しかし、これには2つの問題点があります。

1. すべてのファイルで、このデータ型を指定するのは面倒 (繰り返してはいけない) です
1. 翻訳者はこれらのデータ型の指定に混乱してしまいます

そのため、型指定は主言語ファイルのみ必須です。これは `mkMessage` 関数の3番目の引数で指定します。これはまた、アプリケーションが提供するどの言語もユーザの言語リストに一致しない場合のバックアップ言語の指定にもなります。

## RenderMessage 型クラス

`mkMessage` の呼び出しにより `RenderMessage` 型クラスのインスタンスが作られます。これは Yesod の i18n における核心部です。`RenderMessage` 型クラスは以下のように定義されます。

```haskell
class RenderMessage master message where
    renderMessage :: master  -- ^ type that specifies which set of translations to use
                  -> [Lang]  -- ^ acceptable languages in descending order of preference
                  -> message -- ^ message to translate
                  -> Text

-- | an RFC1766 / ISO 639-1 language code (eg, @fr@, @en-GB@, etc).
type Lang = Text
```

`RenderMessage` クラスには2つのパラメータがあることに注意してください。これはマスターサイトとメッセージ型です。理論的には、今回の場合 master 型については省略可能ですが、これはすべてのサイトは各メッセージ型につき同じ翻訳のセットが必要であることを意味します。 フォームのような共有ライブラリにおいては、それはあまりよく機能しない解決策です。

`renderMessage` 関数はクラスの型パラメータ (master と message) をそれぞれ引数として取ります。残りのパラメータは、優先順位の高い順のユーザが受け入れる言語リストです。このメソッドは、表示可能なユーザが準備した `Text` を返します。

`RenderMessage` の単純なインスタンスには、文字列の実際の翻訳を何も含まない場合があります。代わりに、すべての言語に対し、同じ値を表示します。例えば、

```haskell
data MyMessage = Hello | Greet Text
instance RenderMessage MyApp MyMessage where
    renderMessage _ _ Hello = "Hello"
    renderMessage _ _ (Greet name) = "Welcome, " <> name <> "!"
```

`renderMessage` の初めの2つのパラメータが無視されている点に注意してください。これを複数の言語をサポートするように拡張します。

```haskell
renderEn Hello = "Hello"
renderEn (Greet name) = "Welcome, " <> name <> "!"
renderHe Hello = "שלום"
renderHe (Greet name) = "ברוכים הבאים, " <> name <> "!"
instance RenderMessage MyApp MyMessage where
    renderMessage _ ("en":_) = renderEn
    renderMessage _ ("he":_) = renderHe
    renderMessage master (_:langs) = renderMessage master langs
    renderMessage _ [] = renderEn
```

ここでのアイデアはかなり率直です。各言語に対するヘルパ関数を定義します。そして、それらの言語をキャッチするための節を renderMessage の定義に加えます。最後に、どの言語も一致しなかった時はユーザの優先リストの次の言語の確認に続き、もし、ユーザが指定したすべての言語を網羅しつくしたらデフォルト言語 (今回の場合は英語) を利用するように2つの場合分けを追加する。

しかし、これらのものを手動で書くことを心配する必要はありません。なぜなら、メッセージファイルのインターフェースがこれらすべてを行ってくれるためです。しかし、表面下で何が起こっているかを知っておくことは、常によい考えです。

## 展開

新しい `RenderMessage` のインスタンスを使う方法の1つは直接 `renderMessage` を呼ぶことでしょう。この方法も問題はありませんが、、少し面倒です。ファウンデーション値と、言語リストを手動で渡す必要があります。代わりに Hamlet は特別な i18n の展開である `_{...}` を提供します。

<div class=yesod-book-notice>
なぜアンダースコアなのでしょうか？アンダースコアは i18n においてしっかりと確立された文字で gettext ライブラリで利用されています。
</div>

Hamlet は自動的にそれを `renderMessage` の呼び出しに変換します。Hamlet で `Text` 値の出力が手に入れば `toHtml` 関数を利用して `Html` 値を作ることができます。つまり、特殊文字 (<, &, >) は自動的にエスケープされるということです。

## 単語でなくフレーズ

最後は i18n に関するただの一般的なアドバイスです。ここで、カメを売るアプリケーションがあるとしよう。"カートに4匹のカメを追加しました" や "4匹のカメを購入しました、おめでとうございます!" のように "カメ" という言葉を複数の箇所で使いたいとします。プログラマなら、すぐにコードが再利用できそうだと気づきます。"4匹のカメ" という表現を2箇所で利用しているからです。そこで、メッセージファイルを次のようにするかもしれません。

```
AddStart: You have added
AddEnd: to your cart.
PurchaseStart: You have purchased
PurchaseEnd: , congratulations!
Turtles count@Int: #{show count} #{plural count "turtle" "turtles"}
```

ここで書くのをやめてください！これはプログラミングの観点から見ればとても良いことですが、翻訳はプログラミングではありません。このようにしてしまうと、以下のような問題がいくつも発生します。

- "追加しました。" の前に "カートに入れる" を置く言語もあります
- たぶん "追加しました。" の部分は、1匹あるいは複数匹のカメを追加したかという内容に応じて、異なる方法で構築されるかもしれません
- 同様に空白の問題が数多くあります。

よって、一般的なルールは「単語だけではなく、フレーズ全体を翻訳しよう」です。

## 本書のコード

- [Synopsis.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch016/Synopsis.hs)