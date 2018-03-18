---
title: Forms
date: 2018/03/18
---

## Forms

境界問題については既に説明しました。
データをアプリケーションに入出力する際は常にそのデータを検証する必要があり、おそらくこの必要性が生じる最も難しい場所はフォームです。
フォームのコーディングは複雑です。
理想的な世界では次のような問題を処理する解決策が望まれます。

- データ妥当性の保証
- フォームで提出された文字列データから Haskell データ型への変換
- フォームを表示するための HTML コードの生成
- クライアントサイドで行う妥当性チェックやデイトピッカーのような使い勝手の良いウィジェットを提供するための Javascript 生成
- 単純なフォームを組み合わせることで、複雑なフォームを構築できる機能
- フィールドに一意性が保証されている名称の自動的に割り当て

yesod-form パッケージは宣言的な API の形式でこれらすべての機能を提供しています。
また、フォームの装飾や Javascript の適切な適用を簡略化するために Yesod ウィジェットとして構築されます。
そして、 Yesod の他の機能と同じように Haskell の型システムを利用して全てが正しく動いていることを保証します。

# Synopsis

``` haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text)
import           Data.Time           (Day)
import           Yesod
import           Yesod.Form.Jquery

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/person PersonR POST
|]

instance Yesod App

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- And tell us where to find the jQuery libraries. We'll just use the defaults,
-- which point to the Google CDN.
instance YesodJquery App

-- The datatype we wish to receive from the form
data Person = Person
    { personName          :: Text
    , personBirthday      :: Day
    , personFavoriteColor :: Maybe Text
    , personEmail         :: Text
    , personWebsite       :: Maybe Text
    }
  deriving Show

-- Declare the form. The type signature is a bit intimidating, but here's the
-- overview:
--
-- * The Html parameter is used for encoding some extra information. See the
-- discussion regarding runFormGet and runFormPost below for further
-- explanation.
--
-- * We have our Handler as the inner monad, which indicates which site this is
-- running in.
--
-- * FormResult can be in three states: FormMissing (no data available),
-- FormFailure (invalid data) and FormSuccess
--
-- * The Widget is the viewable form to place into the web page.
--
-- Note that the scaffolded site provides a convenient Form type synonym,
-- so that our signature could be written as:
--
-- > personForm :: Form Person
--
-- For our purposes, it's good to see the long version.
personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs $ Person
    <$> areq textField "Name" Nothing
    <*> areq (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:-5" -- 1900 till five years ago
        }) "Birthday" Nothing
    <*> aopt textField "Favorite color" Nothing
    <*> areq emailField "Email address" Nothing
    <*> aopt urlField "Website" Nothing

-- The GET handler displays the form
getHomeR :: Handler Html
getHomeR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost personForm
    defaultLayout
        [whamlet|
            <p>
                The widget generated contains only the contents
                of the form, not the form tag itself. So...
            <form method=post action=@{PersonR} enctype=#{enctype}>
                ^{widget}
                <p>It also doesn't include the submit button.
                <button>Submit
        |]

-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages.
postPersonR :: Handler Html
postPersonR = do
    ((result, widget), enctype) <- runFormPost personForm
    case result of
        FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{PersonR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

main :: IO ()
main = warp 3000 App
```

## Kinds of Forms

フォームの詳細に入る前に様々な種類のフォームの簡単な説明から始めましょう。
フォームは3種類のカテゴリに分類できます。

### Applicative

これらは最も一般的に利用されます (さきほどの例でも利用されていました)。
アプリカティブは高レベルの宣言的アプローチによって、エラーメッセージを互いに結合するような素晴らしい性質を与えてくれます (アプリカティブコードの詳細については [the Haskell wiki](http://www.haskell.org/haskellwiki/Applicative_functor]) を参照してください。

### Monadic

アプリカティブのより強力な代用品です。
これにより柔軟性は高まりますが、冗長になってしまうというデメリットがあります。
例えば、標準的な2カラムのフォームとは違い、カスタマイズしたレイアウトのフォームを作りたい場合などで利用します。

### Input

フォームの入力を受け取るためだけに利用されます。
また、ユーザから入力を受け取るための HTML は生成しません。
すでにフォームが用意されていて、結果のみを受け取りたい場合に使われます。

更にフォームやフィールドごとにいくつかの異なる変数が必要になるでしょう。

- フィールドは必須かそれとも任意だろうか？
- GET と POST のどちらで提出するべきだろうか？
- デフォルト値があるか、ないか？

最優先の目的はフィールドで定義される変数を最小にして、出来る限り多くの文脈において機能させることです。
これによる1つの結果として、それぞれのフィールドにおける余分な単語を少なくできます。
前の例で `areq` と `Nothing` パラメータに気づいたでしょう。
なぜこれらが必要なのかということについては後で説明しますが、今のところはこれらのパラメータを明示的にすることで (`intField` のような) 個々のフィールドを様々な方法で再利用できるということです。

命名規則の注意点として、各フォームの型は1文字 (A、M、I) の接頭辞を `MForm` のように使います。
また req や opt を必須や任意の意味を表すために利用します。
これらを組み合わせることで `areq` は必須アプリカティブフィールド、`iopt` は任意インプットフィールドを生成します。

## Types

`Yesod.Form.Types` モジュールはいくつかの型を定義しています。
利用可能な全ての型を説明するのではなく、最も重要なものにのみ焦点を当てます。
単純なものから順に説明していきます。

### EncType

エンコーディングタイプは `UrlEncoded` か `Multipart` です。
このデータ型は `ToHtml` 型クラスのインスタンスなので enctype を直接 Hamlet で扱えます。

### FormResult

3つの状態があります。
データが提出されなかった場合の `FormMissing`、フォームのパースにおいてエラーが生じた場合 (例えば、必須フィールドが入力されていない、無効な内容など) の、 `FormFailure`、 全てがうまく行った場合の `FormSuccess` です。 

### FormMessage

データ型として生成することができる全てのメッセージを表します。
例えば `MsgInvalidInteger` はライブラリが与えられたテキスト値が整数でないことを示すために利用されます。
このデータを十分に体系化することで、あらゆる種類のレンダリング関数を提供可能になり、アプリケーションの国際化 (i18n) が可能となります。

次に、個々のフィールドを定義するためのデータ型を見ていきましょう。
フィールドは、数字、文字列、電子メールアドレスのような単一の情報として定義されます。
フィールドが組み合わさってフォームを構築します。

### Field

2つの機能を定義します。
それは、ユーザから入力されたテキスト値を Haskell 値にパースする方法とユーザに表示するウィジェットの生成方法の2つです。
`yesod-form` は `Yesod.Form.Fields` で個々のフィールドを定義しています。

### FieldSettings

FieldSettings は、表示名、任意ツールチップ、ハードコードされた `id` や `name` 属性のようなフィールドの情報の表示方法について定義します (何も与えられなかった場合は、自動生成されます)。
最初の例でも利用したように、 `FieldSettings` は `IsString` 型クラスのインスタンスとなっているため `FieldSettings` 値が必要な場合は、通常の文字列リテラルで入力できます。

最後の重要な部分です。
それはフォーム自身で、これには3つの型が存在します。
モナディックフォームのための `MForm`、アプリカティブフォームのための `AForm`、インプットフォームのための `FormInput` があります。
`MForm` は実際には次の特徴を持つモナドスタックの型シノニムになっています。

- ユーザが提出したパラメータ、ファウンデーションデータ型、ユーザサポートの言語リストを与えるための `Reader` モナド。ファウンデーションデータ型とユーザサポートの言語リストは i18n をサポートするために `FormMessage` のレンダリングで利用されます (詳しくは後で説明します。)
- `Enctype` を追跡するための `Writer` モナド。フォームは常に `UrlEncoded` なので、ファイル入力フィールドがある場合は代わりに `Multipart` を使わなければなりません
- フィールドのために生成された名前や識別子を追跡する `State` モナド

`AForm` もほとんど同じですが、多少の相違点があります。

- ユーザに何を表示するか追跡するために `FieldView` のリストを生成します。これにより、フォームの表示を抽象的な状態に留めておき、最終的にそれをページの下地にするめの適切な関数を選ぶことが可能となります。最初の例では `renderDivs` を利用しましたが、それは div タグの塊を作ります。他の2つの選択肢として `renderBootstrap` と `renderTable` があります。
- アプリカティブフォームは `Monad` のインスタンスではありません。`Applicative` の目的はフォーム全体が起動し各フィールドに関する出来る限り多くの情報を手に入れ, 最終結果を作ることです。これは `Monad` の文脈では機能しません。

`FormInput` はもっと単純で、エラーメッセージのリストか結果を返します。

## Converting

"しかし、少し待ってください。あなたは、最初の例でアプリカティブフォームを使うと言いましたが、型注釈は `MForm` となっているので、モナディックフォームなのではないでしょうか?" と、言いたくなる人もいるでしょう。
その通りです。最終的なフォームはモナディックになっています。
しかし、実際にはアプリカティブフォームからモナディックフォームへの変換が起こっているのです。

目標を再確認しましょう。私たちの目標は、コードを出来る限り再利用し、API における関数の数を最小限にすることでした。
そして、モナディックフォームは少し使いにくい部分もありますが、アプリカティブフォームより強力です。
そのため、アプリカティブフォームで表現できるものは全て、モナディックフォームでも表現可能です。
この変換のためには2つの重要な関数があります。
`aformToForm` はあらゆるアプリカティブフォームをモナディックフォームへ変換し、 `formToAForm` は特定のモナディックフォームをアプリカティブフォームに変換します。

"ちょっと待って。どこを見ても `aformToForm` なんてありませんよ。" とあなたは言うでしょう。
その通りです。`renderDivs` 関数の内部で利用されているんです。

## Create AFormS

最初の例で、実際にはアプリカティブフォームを使っていたことを (おそらく) 納得してもらえたでしょう。
これらが、どのように作られるか確認しながら理解を深めましょう。
以下はフォームを作る簡単な例です。

``` haskell
data Car = Car
    { carModel :: Text
    , carYear  :: Int
    }
  deriving Show

carAForm :: AForm Handler Car
carAForm = Car
    <$> areq textField "Model" Nothing
    <*> areq intField "Year" Nothing

carForm :: Html -> MForm Handler (FormResult Car, Widget)
carForm = renderTable carAForm
```

ここでは、明示的にアプリカティブフォームとモナディックフォームを区別しています。
`carAForm` においては `<$>` と `<*>` 演算子を利用していますが、これは驚くべきことではありません。
アプリカティブ形式のコードはたいていこのようになります。
そしてそれぞれの行が `Car` データ型のレコードと対応しています。
これも驚くべきことではありません。
`Text` レコードには `TextField`、`Int` レコードには `IntField` を利用しています。

もっと注意深く `areq` 関数をみてみましょう。
その (単純化された) 型注釈は `Field a -> FieldSettings -> Maybe a -> AForm a` です。
最初の引数はこのフィールドのデータ型、パース方法、、レンダリング方法を指定します。
また、次の引数の `FieldSettings` は、ラベル、ツールチップ、名前、フィールドのIDを伝えるためのものです。
今回の場合は、先ほど説明した `FieldSettings` が `IsString` のインスタンスだということを利用して FieldSettings を生成しています。

また `Maybe a` は何でしょうか？
それは、任意の初期値です。
例えば、CarYear の初期値を "2007" にする場合は `areq intField "year" (Just 2007)` とします。
さらに、フォーム全体の初期値を考えることもでき、その場合は任意パラメータとしてフォーム全体の初期値を取るようにすれば良いでしょう。

``` haskell
carAForm :: Maybe Car -> AForm Handler Car
carAForm mcar = Car
    <$> areq textField "Model" (carModel <$> mcar)
    <*> areq intField  "Year"  (carYear  <$> mcar)
```

### 任意フィールド

(車の色) のような任意フィールドにしたい場合は areq を `aopt` に置き換えるだけで済みます。

``` haskell
carAForm :: AForm Handler Car
carAForm = Car
    <$> areq textField "Model" Nothing
    <*> areq intField "Year" Nothing
    <*> aopt textField "Color" Nothing
```

そして、必須フィールドのように、最後の引数は任意の初期値です。
しかし、これは2層の Maybe でラッピングされています。
これは、少し冗長ではありますが、次の例で示すように、任意のフォーム初期値を取るコードを綺麗に書けます。

``` haskell
carAForm :: Maybe Car -> AForm Handler Car
carAForm mcar = Car
    <$> areq textField "Model" (carModel <$> mcar)
    <*> areq intField  "Year"  (carYear  <$> mcar)
    <*> aopt textField "Color" (carColor <$> mcar)

carForm :: Html -> MForm Handler (FormResult Car, Widget)
carForm = renderTable $ carAForm $ Just $ Car "Forte" 2010 $ Just "gray"
```

## Validation

どのようにすれば1990年以降に作られた車だけを受け付けるフォームを作ることができるでしょうか？
`Field` は有効な入力値に関する情報を含んでいたことを思い出してください。
検証パターンごとに新しい `Field` を毎回書かなければならないとなると、それは少し面倒です。
なので、既存のコードを修正する方法にしましょう。

``` haskell
carAForm :: Maybe Car -> AForm Handler Car
carAForm mcar = Car
    <$> areq textField    "Model" (carModel <$> mcar)
    <*> areq carYearField "Year"  (carYear  <$> mcar)
    <*> aopt textField    "Color" (carColor <$> mcar)
  where
    errorMessage :: Text
    errorMessage = "Your car is too old, get a new one!"

    carYearField = check validateYear intField

    validateYear y
        | y < 1990 = Left errorMessage
        | otherwise = Right y
```

ここでのトリックは `check` 関数です。
`check` 関数は関数 (`validateYear`) を取り、エラーメッセージか変更されたフィールド値を返します。
この例では、値は全く変更されていません。
それはよくあることです。
この種のチェックは非常に日常的なことなので checkBool という関数が用意されています。

``` haskell
carYearField = checkBool (>= 1990) errorMessage intField
```

`checkBool` は満たすべき条件と、もしそうでなかった場合に表示するエラーメッセージの2つの引数を取ります。

`errorMessage` における明示的な `Text` 型注釈に気づいたでしょう。
`OverLoadedStrings` を利用している場合に必要となります。
i18n をサポートするために、メッセージは多くの異なるデータ型になれますが、GHCは `IsString` のどのインスタンスを使えば良いのか決定できません。

車があまり古すぎないことを確実にできることは素晴らしいです。
しかし、指定された値が将来のものでないことを確かめるにはどうしたら良いでしょうか？
現在の年を探すために何らかの `IO` を実行する必要があります。
このような状況においては `checkM` を使うことで任意のアクションを実行する検証コードが書けるようになります。

``` haskell
    carYearField = checkM inPast $ checkBool (>= 1990) errorMessage intField

    inPast y = do
        thisYear <- liftIO getCurrentYear
        return $ if y <= thisYear
            then Right y
            else Left ("You have a time machine!" :: Text)

getCurrentYear :: IO Int
getCurrentYear = do
    now <- getCurrentTime
    let today = utctDay now
    let (year, _, _) = toGregorian today
    return $ fromInteger year
```

`inPast` は `Handler` モナドに包まれた `Either` 型の結果を返す関数です。
現在の年を得るために `liftIO getCurrentYear` を利用し、それをユーザの入力した年と比較しています。
また、複数の検証をつなぐことができることにも注目してください。

`checkM` 検証機は `Handler` モナドの中で実行されるため、Yesod でたいてい扱われるような多くのものにアクセス可能である.
これは特にデータベースアクションを実行する際に便利です。詳細は Persistent の章で扱います。

## More sophisticated fields

今回のカラーフィールドは良いものですが、十分にユーザーフレンドリーなものとは言えません。
ユーザが期待しているものはドロップダウンリストでしょう。

```haskell
data Car = Car
    { carModel :: Text
    , carYear :: Int
    , carColor :: Maybe Color
    }
  deriving Show

data Color = Red | Blue | Gray | Black
    deriving (Show, Eq, Enum, Bounded)

carAForm :: Maybe Car -> AForm Handler Car
carAForm mcar = Car
    <$> areq textField "Model" (carModel <$> mcar)
    <*> areq carYearField "Year" (carYear <$> mcar)
    <*> aopt (selectFieldList colors) "Color" (carColor <$> mcar)
  where
    colors :: [(Text, Color)]
    colors = [("Red", Red), ("Blue", Blue), ("Gray", Gray), ("Black", Black)]
```

`selectFieldList` はタプルのリストを取ります。
タプルの第1要素はドロップダウンリストでユーザに表示するテキストです。
また、第2要素は実際の Haskell 値です。
もちろん、上のようなコードはかなり繰り返しの多いものに見えます。
GHC が自動的に導出する Enum や Bounded インスタンスを使うことで同じ結果が得られます。

``` haskell
colors = map (pack . show &&& id) [minBound..maxBound]
```

`[minBound..maxBound]` は異なる `Color` 値のリストとなります。
`map` と `&&&` (いわゆるファンアウト演算子) を適用することで、それをタプルのリストにしています。
また、さらにこれは yesod-form により提供される `optionsEnum` 関数を使ってさらに簡略化すると、最終的なコードは次のようになります。

```haskell
carAForm :: Maybe Car -> AForm Handler Car
carAForm mcar = Car
    <$> areq textField "Model" (carModel <$> mcar)
    <*> areq carYearField "Year" (carYear <$> mcar)
    <*> aopt (selectField optionsEnum) "Color" (carColor <$> mcar)
```

ドロップダウンリストよりもラジオボタンを好む人がいるかも知れません。
幸いなことに、これはただ1つのワードを変更するだけで済みます。

```haskell
carAForm = Car
    <$> areq textField                    "Model" Nothing
    <*> areq intField                     "Year"  Nothing
    <*> aopt (radioField optionsEnum) "Color" Nothing
```

## Running forms

どこかのタイミングで、素晴らしいフォームを使って何らかの結果を作り出す必要があります。
このために用意されたいくつもの異なる関数が存在しそれぞれが独自の目的を持っています。
最も一般的なものからはじめて、それらを一通り見てみましょう。

### runFormPost

これは、提出された `POST` パラメータに対してフォームを実行します。
もし、これが `POST` 提出でなければ `FormMissing` が返ります。
これは自動的にセキュリティトークンを hidden フィールドとして挿入し [クロスサイトリクエストフォージェリ](https://en.wikipedia.org/wiki/Cross-site_request_forgery)(CSRF) 攻撃を回避します。

## runFormGet

GETパラメータのための `runFormPost` です。
`GET` 提出による通常の `GET` によるページ読み込みと区別するために、`_hasdata` hidden フィールドをフォームに含めます。
`runFormPost` と異なる点は CSRF 保護がないことです

### runFormPostNoToken

`runFormPost` と同じですが、CSRF セキュリティトークンを含みません (あるいは必要としません)。

### generateFormPost

既存の `POST` パラメータを受け取る代わりに、あたかも何のパラメータもなかったかのように振る舞います。
これは、ウィザードの場合のように前のフォームが提出された後に新しいフォームを生成する場合にとても便利です。

### generateFormGet

`generateFormPost` と同じですが `GET`用です。

はじめの3つにおける結果の型は `((FormResult a, Widget), Enctype)` です。
`Widget` はこの段階で検証エラーと前に提出された値を持っています。

なぜ特定のデータ型の代わりに、ネストしたタプルを用いるのでしょうか？
これは `runFormPostNoToken` と `runFormGet` が `FormResult` や `Widget` を返さないフォームで用いられるようにするためです。
これは (以下で論ずるような) より洗練されたモナドフォームを用いる場合に便利です。

## i18n

今回 i18n は少ししか説明しませんが、別の章でより徹底的に取り上げます。
i18n は `yesod-form` にかなり深い影響をもたらすため、簡単に説明したいと思います。
Yesod において i18n の背景にあるアイデアは、データ型にメッセージを表現させることです。
各サイトは与えられたデータ型に対する `RenderMessage` のインスタンスを持てます。
それは、ユーザが許容する言語リストにもとづいてメッセージを翻訳するものです。
これら全ての結果として多少の注意すべきことがあります。

- 全てのサイトは自動的に `Text` に対する `RenderMessage` のインスタンスになります。従って i18n サポートの心配をしなくてもプレーンな文字列を用いることができます。しかし、明示的な型注釈が必要なときもあります。
- `yesod-form` は全てのメッセージを `FormMessage` データ型として扱います。従って `yesod-form` を用いるためには、適切な `RenderMessage` インスタンス宣言をする必要があります。デフォルトの英語翻訳を用いる例は以下の通りです

```haskell
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
```

これは scaffolded サイトでは、自動的に行われます。

## Monadic Forms

単純なフォームレイアウトで十分な場合はアプリカティブフォームを使うべきです。
しかし、フォームをもっとカスタマイズした見た目にしたいときもあるでしょう。

TODO: 画像入れる。

そういった時に、モナディックフォームを使います。
それらは、アプリカティブの場合よりも少し冗長になりますが、それを受け入れればフォームの見え方を完全にコントロールできるようになります。
上の画像のようなフォームを生成するためには、次のようにコーディングすれば良いでしょう。

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Applicative
import           Data.Text           (Text)
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

data Person = Person
    { personName :: Text
    , personAge  :: Int
    }
    deriving Show

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm extra = do
    (nameRes, nameView) <- mreq textField "this is not used" Nothing
    (ageRes, ageView) <- mreq intField "neither is this" Nothing
    let personRes = Person <$> nameRes <*> ageRes
    let widget = do
            toWidget
                [lucius|
                    ##{fvId ageView} {
                        width: 3em;
                    }
                |]
            [whamlet|
                #{extra}
                <p>
                    Hello, my name is #
                    ^{fvInput nameView}
                    \ and I am #
                    ^{fvInput ageView}
                    \ years old. #
                    <input type=submit value="Introduce myself">
            |]
    return (personRes, widget)

getHomeR :: Handler Html
getHomeR = do
    ((res, widget), enctype) <- runFormGet personForm
    defaultLayout
        [whamlet|
            <p>Result: #{show res}
            <form enctype=#{enctype}>
                ^{widget}
        |]

main :: IO ()
main = warp 3000 App
```

アプリカティブにおける `areq` と同じように、モナディックフォームには `mreq` を使います (また、任意フィールドの `mopt` もあります)。
大きな違いは `mreq` は値のペアを返すことです。
FieldView 値を隠して、自動的にそれをウィジットに挿入する代わりに好きな場所に挿入することができます。

`FieldView`はいくつもの情報を持ちますが、最も重要なものは実際のフォームフィールドとなる `fvInput` です。
この例ではインプットタグの HTML に `id` 属性を追加する `fvId` も利用しました。
今回の例ではフィールドの幅を指定するために指定しました。

"これは使われていません" や "これも使われていません" の値は何になるのだろうかと不思議に思うでしょう。
`mreq` は `FieldSettings` を第2引数に取ります。
`FieldSettings` は `IsString` 型クラスのインスタンスであるので、コンパイラによって文字列は以下のように展開されます。

```haskell
fromString "this is not used" == FieldSettings
    { fsLabel = "this is not used"
    , fsTooltip = Nothing
    , fsId = Nothing
    , fsName = Nothing
    , fsAttrs = []
    }
```

アプリカティブフォームの場合 `fsLabel` と `fsTooltip` の値は HTML を構築するために利用されました。
モナディックフォームの場合は HTML の "ラッパー" は生成されないため、これらの値は無視されます。
しかし、場合によっては `id` や `name` 属性を上書きするために `FieldSettings` パラメータを持ち続けます。

他に面白いものは `extra` の値です。
`GET` フォームは提出されたものであることを示すため、`POST` フォームは CSRF 攻撃を防ぐためにセキュリティトークンをこの値に持ちます。
もし、この追加的な hidden フィールドが含まれない場合、フォームの提出は失敗します。

それ以外は本当にそのままです。
`nameRes` と `ageRes` を組み合わせることで `personRes` の値を作り、 person とウィジェットのタプルを返します。
そして `getHomeR` 関数では全てはアプリカティブフォームと同様です。
実際に、モナディックフォームとアプリカティブフォームを交換してもコードは問題なく動きます。

## Input forms

アプリカティブフォームやモナディックフォームは共に HTML コードの生成とユーザ入力の構文解析を行います。
たまには、既にどこかに HTML フォームが存在したり、Javascript を使って動的にフォームを生成したいような場合やフォームの入力処理のみ行うことがあるかもしれません。
そのような場合にインプットフォームを用います。

これらは、多少の違いを除き大部分がアプリカティブフォームとモナディックフォームと同様に機能します。

- `runInputPost` と `runInputGet` を利用します
- `ireq` と `iopt` を利用します。これらの関数は2つの引数しか取りません。フィールド型と該当するフィールドの名前 (すなわちHTMLの `name` 属性) です。
- フォームを実行した後は値を返します。ウィジェットやエンコーディングタイプは返しません。
- もし、何らかの検証エラーがあった場合、ページは "無効な引数" のエラーページを返します。

インプットフォームを使って先ほどの例を再構築できます。
しかし、インプットバージョンはそれほどユーザーフレンドリーでないことに注意してください。
もし、アプリカティブフォームやモナディックフォームで入力を間違えた場合、前に入力した値と何を修正するかについての説明を持って、同じページに戻ることができます。
インプットフォームでは、ユーザにはエラーメッセージが表示されるのみです。

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Applicative
import           Data.Text           (Text)
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/input InputR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

data Person = Person
    { personName :: Text
    , personAge  :: Int
    }
    deriving Show

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <form action=@{InputR}>
            <p>
                My name is
                <input type=text name=name>
                and I am
                <input type=text name=age>
                years old.
                <input type=submit value="Introduce myself">
    |]

getInputR :: Handler Html
getInputR = do
    person <- runInputGet $ Person
                <$> ireq textField "name"
                <*> ireq intField "age"
    defaultLayout [whamlet|<p>#{show person}|]

main :: IO ()
main = warp 3000 App
```

## Custom fields

Yesod が提供しているフィールドでかなり広範なフォームのニーズを満たすでしょう。
しかし、時にはそれで満足しないこともあるでしょう。その時は自分で Yesod に新規フィールドを作ることができます。
`Field`のコンストラクタは3つの値を取ります。
`fieldParse` はユーザが提出した値のリストを取り、次の3つのどれかを返します。

- 検証が失敗したことを示すエラーメッセージ
- パースされた値
- Nothing、すなわちデータが与えられなかったことを表します

最後のケースは驚くべきことのように聞こえるかも知れません。
インプットリストが空の時 Yesod が自動的に情報が与えられなかったと判断するように見えるからです。
しかし、実際にはあるフイールド型においてはインプットが無いことは実質的には妥当なインプットになります。
例えば、チェックボックスは空リストが送信されることでチェックされない状態を表します。

また、どうしてリストなのでしょうか？  `Maybe` の方が良いのではいのでしょうか？
そんなことはありません。グループ化されたチェックボックスや、複数選択リストについては同じ名前で複数のウィジッドを持ちます。
下の例では、このトリックを利用しています。

コンストラクタの2つ目の値は `fieldView` です。
これはウィジェットをレンダリングしてユーザに表示します。
この関数は次の引数を取ります。

1. `id` 属性
1. `name` 属性
1. 他の任意の属性
1. 結果は `Either` 値として返されます。これは (パーシングが失敗した場合は) まだパースが済んでいない入力かパースに成功した値をです。`intField` はこの機構を示す素晴らしい例です。 `42` を入力すれば結果の値は `Right 42` になりますが、`turtle` を入力すると結果は `Left "turtle"` になります。これらの値をインプットタグのvalue 属性に変更することでユーザビリティを損なわないようにできます
1. フィールドが必須かを示す `Bool` 値

コンストラクタの最後の値は `fieldEnctype` で、ファイルアップロードを扱う場合に `Multipart` になり、その他の場合は `UrlEncoded` となります。

簡単な例として、フィールドのパスワードを確認するための新規フィールド型を作ってみましょう。
このフィールドは同じ属性の2つのテキスト入力ボックスがあり、値が一致しない場合にエラーメッセージを返します。
他のフィールドと違ってインプットタグに value 属性を設定しないことに注意してください。
なぜなら、HTML にユーザの入力したパスワードを再び表示したくないからです。

```haskell
passwordConfirmField :: Field Handler Text
passwordConfirmField = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [a, b]
                | a == b -> return $ Right $ Just a
                | otherwise -> return $ Left "Passwords don't match"
            [] -> return $ Right Nothing
            _ -> return $ Left "You must enter two values"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
            <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type=password>
            <div>Confirm:
            <input id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=password>
        |]
    , fieldEnctype = UrlEncoded
    }

getHomeR :: Handler Html
getHomeR = do
    ((res, widget), enctype) <- runFormGet $ renderDivs
        $ areq passwordConfirmField "Password" Nothing
    defaultLayout
        [whamlet|
            <p>Result: #{show res}
            <form enctype=#{enctype}>
                ^{widget}
                <input type=submit value="Change password">
        |]
```

## Values that don't come from the user

ブログホスティングウェブアプリケーションを書いていて、ユーザがブログ投稿を入力するためのフォームを作らなければならないとしましょう。
ブログ投稿は以下の4つの情報とします。

- タイトル
- HTMLコンテンツ
- 著者のユーザID
- 公開日

ユーザには始めの2つの値を入力してもらいたいですが、残り2つはそうではそうではありません。
ユーザ ID はユーザを認証すること (この話題についてはまだ触れていない) で自動的に決定されて欲しく、公開日は現在時刻となって欲しいです。
問題となるのはどのようにして単純なアプリカティブフォーム構文を保ちつつ、ユーザから入力されない情報を設定するかです。

答えとしては、2つの補助関数を利用することです。

- `pure` を使えば、通常の値をアプリカティブフォームの値にラップできます
- `lift` を使えば、アプリカティブフォームで、任意の `Handler` アクションの実行が可能になります

これら2つの関数を用いた例を見てみよう.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Time
import           Yesod

-- In the authentication chapter, we'll address this properly
newtype UserId = UserId Int
    deriving Show

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

type Form a = Html -> MForm Handler (FormResult a, Widget)

data Blog = Blog
    { blogTitle    :: Text
    , blogContents :: Textarea
    , blogUser     :: UserId
    , blogPosted   :: UTCTime
    }
    deriving Show

form :: UserId -> Form Blog
form userId = renderDivs $ Blog
    <$> areq textField "Title" Nothing
    <*> areq textareaField "Contents" Nothing
    <*> pure userId
    <*> lift (liftIO getCurrentTime)

getHomeR :: Handler Html
getHomeR = do
    let userId = UserId 5 -- again, see the authentication chapter
    ((res, widget), enctype) <- runFormPost $ form userId
    defaultLayout
        [whamlet|
            <p>Previous result: #{show res}
            <form method=post action=@{HomeR} enctype=#{enctype}>
                ^{widget}
                <input type=submit>
        |]

postHomeR :: Handler Html
postHomeR = getHomeR

main :: IO ()
main = warp 3000 App
```

ここで紹介したトリックの1つは `GET` と `POST` リクエストメソッドで同じハンドラコードを利用することです。
これは `runFormPost` の実装が可能としています。
`runFormPost` は `GET` リクエストの場合に `generateFormPost` と全く同じように振る舞うからです。
両方のリクエストにおいて同じハンドラを利用することでボイラープレートを削減できmasu.

## Summary

Yesod におけるフォームは3つのグループに分けられる。
アプリカティブフォームは使いやすい API と共に素晴らしいユーザインターフェースを提供する。
そして最も一般的であるモナディックフォームはより強力ですが、使いづらいフォームです。
インプットフォームはユーザからのデータを処理するだけで、入力用のウィジットを生成したくない場合に便利です。

Yesod によリ提供される独創的なフィールドがたくさんあります。
これらをフォームで使うために、フォームの種類とフィールドが必須か任意かについて決める必要があり、全部で6つの補助関数 `areq`、`aopt`、`mreq`、`mopt`、`ireq`、`iopt` が用意されています。

フォームでは絶大な表現力が利用できます。
動的に Javascript を挿入することで jQuery UI デイトピッカーのような素晴らしいUIコントロールを利用できます。
また、フォームは完全に i18n に準拠するためグローバルユーザコミュニティをサポートできます。
さらに、より特別なニーズがある場合は存在するフィールドに対する検証関数を増強したり、ゼロから新しい関数を書くこともできます。