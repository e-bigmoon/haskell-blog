---
title: 国際化
date: 2019/01/20
---

# 国際化

ユーザはソフトウェアが自国の言語を話すことを期待している. 我々にとって都合が悪いことに, 1つ以上の言語が関連するのである. 単に文字列を置換するだけのことは複雑ではないが, あらゆる文法の問題を適切に扱うことは, 技巧的である. 結局, '1つのファイルを上げなさい'というプログラムアウトプットは誰が見たいでしょうか?

しかし, リアルなi18nによる解決策は, 単に適切なアウトプットを達成する方法を与える以上のことを要する. それは, この過程をプログラマと翻訳者にとって容易で, 比較的エラープルーフにする必要がある. この問題に対するYesodの答えは次のようである:

- リクエストヘッダに基づき, オーバーライドする能力を用いて, 知的にユーザの望む言語を推測する. 

- Haskellの知識を必要としない翻訳を与える単純な構文.(結局のところ, 大部分の翻訳者はプログラマでない.)

- 大部分の必要性をカバーするデフォルトの補助関数の選択とともに, 必要に応じて技巧的な文法の問題に対しては, Haskellの最大限の力を利用する能力.

- 文字の順序に対しては全く問題ない.

## Synopsis

``` haskell
-- @messages/en.msg
Hello: Hello
EnterItemCount: I would like to buy:
Purchase: Purchase
ItemCount count@Int: You have purchased #{showInt count} #{plural count "item" "items"}.
SwitchLanguage: Switch language to:
Switch: Switch
```

``` haskell
-- @messages/he.msg
Hello: שלום
EnterItemCount: אני רוצה לקנות:
Purchase: קנה
ItemCount count: קנית #{showInt count} #{plural count "דבר" "דברים"}.
SwitchLanguage: החלף שפה ל:
Switch: החלף
```

``` haskell
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

独特でもっとも刺激的なi18nによる解決策は, gettextやJavaのmessage bundleのように, 文字列の検索に基づき機能する. たいていある形式のprintf-展開が変数を文字列に挿入するために用いられる. 推測する通り, Yesodにおいては代わりに型に頼っている. これはコンパイラが自動的に誤りを捕えることのように, あらゆる標準的な利点がある. 

具体例を取り上げてみましょう. アプリケーションがユーザに言いたいことが2つあるとしましょう: helloと言うことと, 何人のユーザはシステムにログインしているか. これは, 直和型を用いてモデル化される:

`data MyMessage = MsgHello | MsgUsersLoggedIn Int`

このデータ型を英語表現に変換するための関数も書くこともできる:

``` haskell
toEnglish :: MyMessage -> String
toEnglish MsgHello = "Hello there!"
toEnglish (MsgUsersLoggedIn 1) = "There is 1 user logged in."
toEnglish (MsgUsersLoggedIn i) = "There are " ++ show i ++ " users logged in."
```

ほかの言語についても同様な関数を書くことができる. この内部Haskell的な方法による利点は, , 特に複数性のような, 技巧的な部方的問題を解決するためにHaskellの力を最大限に利用することができることである. 

<div class=yesod-book-notice>
複数性はそれほど複雑ではないと思うかもしれない: 1つのアイテム関しては1つの方法があり, 他の数に関しては, 他の方法がある. それは英語については本当かもしれないが, すべての言語についてそうであるとは限らない. 例えば, ロシア語は6つの異なる形式があり, どれを用いるかについてモジュール的なロジックを用いる必要がある.
</div>

しかし, 欠点としてはこれらすべてをHaskellの内部に書かなくてはならず, 翻訳者にとってあまり便利でない点である. これを解決するために, Yesodはメッセージファイルの概念を導入している. それについては, 後ほど紹介する.

これら一式の翻訳関数を持っていると仮定した場合, それらをどのように使ったら
よいであろうか? 必要なものとしては, それらすべてをラップした新しい関数であり, ユーザの選択しら言語に基づいて, 適切な翻訳関数を選択することである. 一旦その機能を獲得すれば, 自動的にもっとも関連するレンダリング関数を選び, 与えた値に基づいてそれを呼び出すことができる. 

多少物事を単純化するために, Hamletは特別な展開構文`_{...}`を持ち, それはレンダリング関数に対するあらゆる呼び出しを扱う. そして, レンダリング関数をアプリケーションに関連付けるために, `YesodMessage`型クラスを用いる.

## メッセージファイル

翻訳を作るための最も単純な方法は, メッセージファイルを用いることである. 設定は簡単である: 各言語につき1つのファイルで, すべての翻訳を含んだファイルを含む1つのフォルダがある. 各ファイルはen.msgなどのような, 言語コードに基づき命名される. そして, ファイルの各行は1つの表現を処理し, それはメッセージデータ型の単一のコンストラクタに紐付けられる. 

<div class=yesod-book-notice>
scaffoldedサイトはすでに完成したメッセージフォルダを含む.
</div>

まず初めに, 言語コードについて話す. 実際には2つの利用可能な方法がある: 2文字の言語コードを用いる, または, 言語-地域コードを用いる. 例えば, ページをブラウザにロードする際, 2つの言語コードを送信するものとする: en-USとen: ブラウザが意味しているのは, "もしアメリカ英語があれば, それが最もよい. もし英語があれば, それを代わりにとる"ということである. 

そこで, アプリケーションにおいてどちらの形式を用いればよいであろうか? もし, 実際に地域毎に異なる翻訳を作っているのでなければ, ほとんどの場合, 2文字のコードを用いるであろう. このことは, カナダ英語を求めている人でも英語を見ることを意味する. この裏で, Yesodは関連する2文字のコードを付加する. 例えば, ユーザが次の言語リストを所持しているとする:

```
pt-BR, es, he
```

これが意味することは, "ブラジル系ポルトガル語が好きであり, そして, スペイン語, ヘブライ語である". アプリケーションがpt(一般的なポルトガル語)と英語を, 英語をデフォルトで提供するとする. ユーザの言語リストに厳密に従えば, ユーザは英語を与えられることになる. Yesodでは代わりに, その言語リストを次のように変換する;

```
pt-BR, es, he, pt
```

言い換えれば, 地域毎に異なる翻訳を与えていなければ, 2文字の言語コードに従う.

それでは, メッセージファイルについてはどうであろうか? HamletやPersistentに取り組んだ後では, 構文は非常に馴染み深いものである. 行は, メッセージ名から始まる. これはデータコンストラクタであるため, 大文字で始まる必要がある. 次に, 個々のパラメータを持つことができ, それらは小文字で与えられる. これはデータコンストラクタに対する引数になる. 引数リストはコロンで終了し, そして, 翻訳される文字列が続き, ここでは, 複数性などのような問題を扱うために, 典型的な変数展開構文の翻訳補助関数を用いることができ, すべての必要な翻訳されたメッセージを作ることができる.

## 型を指定する

データ型はメッセージの指定により作られるため, データコンストラクタに対する各パラメータは, データ型を与えられる必要がある. このために@-構文が用いられる. 例えば, `data MyMessage = MsgHello | MsgSayAge Int`のデータ型を作るために, 次のように書く.

``` haskell
Hello: Hi there!
SayAge age@Int: Your age is: #{show age}
```

しかし, これには2つの問題点がある.

1. すべてのファイルにおいて, このデータ型を指定するのは面倒(繰り返してはいけない)である. 

2. 翻訳者はこれらのデータ型を指定するのに混乱してしまう. 

そこで, 代わりに型指定は主言語ファイルのみで必要となる. これは, `mkMessage`関数の3番目の引数として指定される. これはまた, アプリケーションによって与えられたどの言語もユーザの言語リストに一致しない場合に用いられる, バックアップ言語が何であるかを指定する. 

## RenderMessage型クラス

`mkMessage`の呼び出しにより, `RenderMessage`型クラスのインスタンスが作られ, これはYesodのi18nにおける核心部である. それは, 次のように定義される:

``` haskell
class RenderMessage master message where
    renderMessage :: master
                  -> [Lang] -- ^ languages
                  -> message
                  -> Text
```

`RenderMessage`クラスには2つのパラメータが存在することに留意しなさい: それは, マスタサイトと, メッセージ型である. 理論的には, ここではマスター型については省略可能であるが, これはすべてのサイトは, 各メッセージ型につき, 同じ翻訳のセットが必要であることを意味する. フォームのような, 共有ライブラリにおいては, それはあまりよく機能しない解決策である. 

`renderMessage`関数はクラスの型パラメータそれぞれに対するパラメータを取る: マスタ とメッセージである. 残りのパラメータは, ユーザが受け入れる言語リストが, 優先順位に並ぶ. このメソッドは, ユーザに準備した`Text`を返す. 

`RenderMessage`の単純なインスタンスは, 実際の文字列に対する翻訳を含まない; 代わりに, すべての言語に対し, 同じ値を表示する. 例えば, 

``` haskell
data MyMessage = Hello | Greet Text
instance RenderMessage MyApp MyMessage where
    renderMessage _ _ Hello = "Hello"
    renderMessage _ _ (Greet name) = "Welcome, " <> name <> "!"
```

`renderMessage`の初めの2つのパラメータがどのように無視されているかについて注意しなさい. これを複数のメッセージをサポートするように拡張できる:

``` haskell
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

ここでのアイデアはかなり率直である: 各言語に対するヘルパ関数を定義する. そして, これらの言語をキャッチするための節を, renderMessageの定義に加える. すると最後2つの場合が生じる: どの言語も一致しなかったら, ユーザの優先リストにおける次の言語の確認に続く. もし, ユーザが指定したすべての言語を網羅しつくしたら, デフォルト言語(今回の場合は英語)を用いる. 

しかし, これらのものを手動で書くことを心配する必要はないだろう. なぜならば, メッセージファイルのインターフェースがこれらすべてを行ってくれるためである. しかし, 表面下で何が起こっているかを知っておくことは, 常によい考えである. 

## 展開

新しい`RenderMessage`のインスタンスを使う方法は, 直接`renderMessage`を呼ぶことであろう. これは正しいが, 少し面倒である: ファウンデーション値と, 言語リストを手動で渡す必要がある. 代わりに, Hamletは特別なi18nの展開である`_{...}`を提供する. 

<div class=yesod-book-notice>
なぜアンダースコアなのか? アンダースコアは, i18nにおいてしっかりと確立した文字であり, gettextライブラリで用いられる. 
</div>

すると, Hamletは自動的にそれを`renderMessage`の呼び出しに翻訳する. 一度Hamletが`Text`値の出力を得れば, `toHtml`関数を用いて, `Html`値を作る. つまり, 特別な文字(<, &, >)は自動的にエスケープされる. 


## 単語でなく, フレーズ

最後の注意点として, i18に関する一般的なアドバイスを与えたい. カメを売るアプリケーションがあるとしよう. "カートに4匹のカメを追加しました."や, "4匹のカメを購入しました, おめでとうございます!"のように, "カメ"という言葉を複数の箇所で使うだろう. プログラマとして, すぐにコード再利用の可能性に気づくだろう: "4匹のカメ"という表現を2箇所で用いている. そこで, メッセージを次のように構築するだろう:

```
AddStart: You have added
AddEnd: to your cart.
PurchaseStart: You have purchased
PurchaseEnd: , congratulations!
Turtles count@Int: #{show count} #{plural count "turtle" "turtles"}
```

ここで書くのをやめなさい! これはプログラミングの観点からすると, とてもよいことであるが, 翻訳はプログラミングではない. このようにすると, 以下に示すように余りうまくいかないことが生じてくる:

- "追加する前に", "カートに入れる"言語もある.

- "追加した"の部分は, 1匹あるいは複数匹のカメを追加したかにより, 異なる方法で構築されるかもしれない.

- 同様に空白の問題が数多くある.

よって, 一般的なルールとして, 単に単語ではなく, フレーズを翻訳しなさい.