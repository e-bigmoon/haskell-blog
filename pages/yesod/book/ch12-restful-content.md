---
title: RESTful Content
date: 2018/09/22
---

Web の初期からの話の1つに、検索エンジンがどのようにして Web サイトを消去してしまったかという話があります。まだ動的な Web サイトが新しい概念だった頃、開発者は `GET` と `POST` リクエストの違いについて、よく理解していませんでした。その結果、`GET` メソッドでアクセスするとページが削除されてしまうようなページが作られました。検索エンジンがこのようなサイトをクローリングすれば、全てのコンテンツが消去されてしまいます。

もし、Web 開発者が HTTP の仕様に適切にしたがっていれば、このようなことは起こらなかったでしょう。なぜなら `GET` リクエストは副作用を引き起こさないことになっているからです (ご存知のように、それはサイトを消去することなどです)。最近の Web 開発では Representational State Transfer (REST) を適切に取り入れようという動きがあります。この章では、Yesod の RESTful な側面や、より堅牢な Web アプリケーションを作るために、それらをどのように利用すれば良いかについて説明します。

## リクエストメソッド

多くの Web フレームワークでは、リソースごとにハンドラ関数を作ります。Yesod のデフォルトでは、リクエストメソッドごとにハンドラ関数を作ります。Web サイトを構築する際のもっとも一般的なリクエストメソッドは `GET` と `POST` の2つです。この2つのリクエストメソッドは Web フォームがサポートしている唯一のメソッドなので、HTML で最も多くサポートされています。しかし、RESTful API を作るのであれば、その他のメソッドもとても便利です。

技術的な話で言えば、どのメソッドでも好きに使うことができます。しかし、HTTP の仕様を遵守することが強く推奨されます。よく使われるメソッドは次のとおりです。

### GET

読み取り専用のリクエスト。サーバ側に全く変化が起こらないことを想定しているため、"現在時刻" やランダムな結果などを除けば、`GET` リクエストを何回呼び出しても同じレスポンスが返ってきます。

### POST

一般的に変化のあるリクエストです。`POST` リクエストはユーザによって2度提出されるべきではありません。よくある例として、ある銀行口座から別のところへ資金を送金する例などがあります。

### PUT

サーバに新しいリソースを作成する、または、既存のものを置き換えます。このメソッドは何度呼ばれても安全です。

### PATCH

サーバ上のリソースを部分的に更新します。リソースの1つ以上のフィールドを更新したいときに、このメソッドを使うべきでしょう。

### DELETE

文字通り、サーバ上のリソースを削除します。何度呼び出しても問題が起こらないようにするべきです。

ある程度までは、Haskell の哲学に非常によくフィットします。`GET` リクエストは副作用を持たない点などで純粋関数に似ています。実際には `GET` 関数はおそらく、データベースからの情報の読み出し、ユーザ操作の記録などの `IO` を行っているでしょう。

リクエストメソッドごとにハンドラ関数を定義する構文については、ルーティングとハンドラの章を参照してください。

## 表現

次のような Haskell の型と値があったとしましょう。

```haskell
data Person = Person { name :: String, age :: Int }
michael = Person "Michael" 25
```

このデータを HTML で表現することもできます。

```html
<table>
    <tr>
        <th>Name</th>
        <td>Michael</td>
    </tr>
    <tr>
        <th>Age</th>
        <td>25</td>
    </tr>
</table>
```

また、JSON で表現すれば以下のようになります。

```json
{"name":"Michael","age":25}
```

さらに XML ではこうなります。

```xml
<person>
    <name>Michael</name>
    <age>25</age>
</person>
```

たまにですが、Web アプリケーションは上記のそれぞれの表現を提供するために、それぞれ別のURLを利用する場合があります。たいていは `/person/michael.html` や `/person/michael.json` などになるでしょう。Yesod は RESTful 原則に従って、すべてのリソースを1つのURLで提供します。そのため、Yesod では `/person/michael` にアクセスすれば全ての表現を取得できます。

ここで気になるのは、**どの**表現かの決定方法をどうするかということです。答えは、HTTP `ACccept` ヘッダです。このヘッダに、クライアントが期待するコンテンツタイプの優先度付きリストを指定します。Yesod は関数のペアを持つことで、直接ヘッダーをパーズせず、より高レベルの表現で扱うことができます。最後の文章をコードでより具体化してみましょう。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text (Text)
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    provideRep $ return
        [shamlet|
            <p>Hello, my name is #{name} and I am #{age} years old.
        |]
    provideRep $ return $ object
        [ "name" .= name
        , "age" .= age
        ]
  where
    name = "Michael" :: Text
    age = 28 :: Int

main :: IO ()
main = warp 3000 App
```

`selectRep` 関数は "これらの表現が選択可能ですよ" という意味です。それぞれの `provideRep` の呼び出しは、代わりの表現を提供します。Yesod は Haskell の型によって各表現の MIME タイプを決定します。`shamlet` (simple Hamlet) は `Html` 型の値を生成し、Yesod は関連する MIME タイプが `text/html` だとわかります。同様に `object` は JSON 型の値を生成し、MIME タイプが `application/json` だとわかります。`TypedContent` は Yesod が提供している型で、MIME タイプ付きの生コンテンツのためにあります。

これを試すために、次の異なる `curl` コマンドを実行してみましょう。

```shell
curl http://localhost:3000 --header "accept: application/json"
curl http://localhost:3000 --header "accept: text/html"
curl http://localhost:3000
```

accept ヘッダ値によってどのようにレスポンスが変化するでしょうか。また、ヘッダを省略すると、HTML レスポンスがデフォルトで表示されます。ルールとして、accept ヘッダが無ければ一番初めの表現 (この場合では html) が表示されます。また、accept ヘッダはあるけれども、当てはまるものがなければ 406 "アクセスできません" レスポンスが返されます。

esod はデフォルトでクエリ文字列パラメータで accept ヘッダを設定するための便利なミドルウェアを提供しています。これを使うことで、ブラウザから簡単にテストを実施できます。この機能を試すためには [http://localhost:3000/?_accept=application/json](http://localhost:3000/?_accept=application/json) にアクセスしてください。

## JSON の利便性

JSON は Web アプリケーションのデータフォーマットとして一般的に利用されているため、Yesod には JSON 表現を提供するためのヘルパー関数が組み込まれています。これらは、素晴らしい `aeson` ライブラリで構成されています。そのため、まずはライブラリの機能についての簡単な説明から始めましょう。

`aeson` の核となる型は、あらゆる有効な JSON の値を表現するための `Value` 型です。また、`ToJSON` と `FromJSON` の2つの型クラスが JSON 値の相互変換を自動的に行います。目的を達成するためには、今の所 `ToJSON` に興味があります。これまでに繰り返し出てきた `Person` 型を使って `ToJSON` インスタンスを作る簡単な例を見てみましょう。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Text                  (Text)

data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]

main :: IO ()
main = L.putStrLn $ encode $ Person "Michael" 28
```

`aeson` については、これ以上深入りしないようにしましょう。なぜなら、[Haddock ドキュメント](https://www.fpcomplete.com/haskell) にライブラリに良いイントロダクションがあるためです。Yesod の便利な関数を理解するためであれば、このぐらいで十分でしょう。

`Person` 型と、その型に対応する値があり、現在のページにその表現を利用したいとしましょう。そのためには `returnJson` 関数を利用します。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text (Text)
import           Yesod

data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Value
getHomeR = returnJson $ Person "Michael" 28

main :: IO ()
main = warp 3000 App
```

`returnJson` は非常に単純な関数で、`return . toJSON` として実装されています。しかし、この関数を使えばちょっとだけ便利になります。同じように、JSON 値を `selectRep` の内部で表現として与えたい場合は `provideJson` 関数を利用します。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text (Text)
import           Yesod

data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    provideRep $ return
        [shamlet|
            <p>Hello, my name is #{name} and I am #{age} years old.
        |]
    provideJson person
  where
    person@Person {..} = Person "Michael" 28

main :: IO ()
main = warp 3000 App
```

`provideJson` は `provideRep . return . toEncoding` として実装されています。

## 新しいデータ型

Haskell の `Show` インスタンスに基づく新しいデータ形式を思い付いたとします。それを "Haskell Show" と呼び、MIME タイプに `text/haskell-show` を与えます。そして、この表現を自分の Web アプリケーションに含めることに決めました。Yesod でこれをどのように行えば良いのでしょうか？まずは、ためしに `TypedContent` 型を直接使ってみましょう。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text (Text)
import           Yesod

data Person = Person
    { name :: Text
    , age  :: Int
    }
    deriving Show

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

mimeType :: ContentType
mimeType = "text/haskell-show"

getHomeR :: Handler TypedContent
getHomeR =
    return $ TypedContent mimeType $ toContent $ show person
  where
    person = Person "Michael" 28

main :: IO ()
main = warp 3000 App
```

この例には、いくつか重要な点があります。

- `toContent` 関数を利用しました。`toContent` は型クラスのメソッドとなっているため、いくつものデータ型を送信可能な生データに変換できます。今回の例では、`String` のインスタンスを利用しました。その場合は UTF8 エンコーディングが行われます。インスタンスとなっている型には他に `Text`、`ByteString`、`Html` や aeson の `Value` などがあります。
- `TypedContent` コンストラクタを直接利用しています。このコンストラクタは MIME タイプと生コンテンツの2つの引数を取ります。また、`ContentType` は正格な `ByteString` のエイリアスとして定義されています。

この例でも良いかもしれませんが、`getHomeR` の型注釈の情報量が少なすぎることは悩ましいことです。また、`getHomeR` の実装はかなりボイラーブレートのように見えます。この方法ではなく、"Haskell Show" データを表現するデータ型と、その値を作るための簡単な方法を提供したいです。試してみましょう。

```haskell
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
import           Data.Text (Text)
import           Yesod

data Person = Person
    { name :: Text
    , age  :: Int
    }
    deriving Show

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

mimeType :: ContentType
mimeType = "text/haskell-show"

data HaskellShow = forall a. Show a => HaskellShow a

instance ToContent HaskellShow where
    toContent (HaskellShow x) = toContent $ show x
instance ToTypedContent HaskellShow where
    toTypedContent = TypedContent mimeType . toContent

getHomeR :: Handler HaskellShow
getHomeR =
    return $ HaskellShow person
  where
    person = Person "Michael" 28

main :: IO ()
main = warp 3000 App
```

ここでのトリックは、2つの型クラスにあります。少し前に説明したように、`ToContent` は値を生レスポンスに変換する方法について言っています。今回の場合、もとの値を `show` することで `String` 型に変換し、`String` を生コンテンツに変換したいです。今回の例のように `ToContent` のインスタンスは相互依存することが多いです。

`ToTypedContent` は Yesod の内部で利用されているため、全てのハンドラ関数の結果で呼び出されます。見ての通り実装はかなりシンプルです。単に MIMEタイプを定義し、`toContent` を呼び出してるだけです。

最後に、もう少し複雑な例として `selectRep` と上手く連携させてみましょう。

```haskell
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
import           Data.Text (Text)
import           Yesod

data Person = Person
    { name :: Text
    , age  :: Int
    }
    deriving Show

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

mimeType :: ContentType
mimeType = "text/haskell-show"

data HaskellShow = forall a. Show a => HaskellShow a

instance ToContent HaskellShow where
    toContent (HaskellShow x) = toContent $ show x
instance ToTypedContent HaskellShow where
    toTypedContent = TypedContent mimeType . toContent
instance HasContentType HaskellShow where
    getContentType _ = mimeType

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    provideRep $ return $ HaskellShow person
    provideJson person
  where
    person = Person "Michael" 28

main :: IO ()
main = warp 3000 App
```

ここで例に追加された重要な点は `HasContentType` インスタンスです。これは、冗長に見えるかもしれませんが、重要な役割を担います。***表現を作る前***に、起こりうる表現の MIME タイプを決定できなければなりません。`ToTypedContent` は具体的な値にのみ機能するため、値を作る前には利用できないのです。`getContentType` は代わりにプロキシー値を取るので、具体的な値が何も無くても MIME タイプを示すことができます。

<div class=yesod-book-notice>
`HasContentType` のインスタンスではない値に対して表現を与えたい場合は `provideRepType` 関数が利用できます。この関数は明示的に MIME タイプを提供する必要があります。
</div>

## 他のリクエストヘッダ

利用可能なリクエストヘッダは他にも数多く存在します。その内のいくつかは、サーバとクライアント間でのデータの移動にのみ影響し、アプリケーションには全く影響を与えません。例えば、`Accept-Encoding` はサーバにどの圧縮方法をクライアントが理解できるかを伝え、`Host` はサーバーが待ち受けている仮想ホストを伝えます。

他のヘッダはアプリケーションに影響を与えますが、Yesod によって自動的に読み込まれます。例えば `Accept-Language` ヘッダは、どの言語 (英語、スペイン語、ドイツ語、スイス系ドイツ語) をクライアントが求めるかを指定します。このヘッダの使い方については i18n の章を参照してください。

## まとめ

Yesod は次の REST 原則に従います。

- 適切にリクエストメソッド使います。
- 各リソースは確実に1つのURLを持ちます。
- 同じURLでデータを複数の表現で提供できます。
- リクエストヘッダを調べることで、クライアントが何を望んでいるかに関する追加情報を判断します。

これらは Yesod を使って Web サイトを構築するだけでなく、API を作ることも簡単になります。実際に、`selectRep/provideRep` のようなテクニックを利用することで、ユーザフレンドリーな HTML ページと、マシンフレンドリーな JSON ページを同じURLで提供できるようになります。

## 本章のコード

- [Representations.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch12/Representations.hs)
- [JSON_conveniences1.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch12/JSON_conveniences1.hs)
- [JSON_conveniences2.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch12/JSON_conveniences2.hs)
- [JSON_conveniences3.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch12/JSON_conveniences3.hs)
- [New_datatypes1.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch12/New_datatypes1.hs)
- [New_datatypes2.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch12/New_datatypes2.hs)
- [New_datatypes3.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch12/New_datatypes3.hs)
