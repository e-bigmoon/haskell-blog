# RESTful Content

Webの初期における話の一つに, どのようにサーチエンジンがウェブサイト全体を見渡していたかというものがある. まだ動的なウェブサイトが新しい概念であった頃は, 開発者は`GET`と`POST`リクエストの違いを認識していなかった.
その結果, `GET`メソッドでアクセスされ, ページを消すようなページが作られた. サーチエンジンがこのようなサイトをスクリーニングするようになると, そのようなサイトは消去されてしまうようになった.

もしこれらの開発者がHTTP仕様に適切にしたがっていれば, このようなことは起こらなかったであろう. `GET`リクエストは, 副作用を起こさないことになっている(ご存知のように, それはサイトを消去することなどである). 最近, Web開発において, RESTとしても知られている, Representational State Transferを適切に採用する動きがある. この章では, YesodにおけるRESTfulな側面と, それらを用いてどのようにして, より堅牢なウェブアプリケーションを作ることができるか, について説明する.

## リクエストメソッド

多くのウェブフレームワークにおいては, 1つのリソースにつき, 1つのハンドラ関数を作る. Yesodにおいては, 異なるリクエストメソッドについて, 異なるハンドラ関数を作ることがデフォルトとなっている. ウェブサイトを構築するにあたり扱われる, もっとも一般的な2つのリクエストメソッドは, `GET`と`POST`である. これらは, HTMLにおいて, もっともよくサポートされているメソッドである. 何故ならば, ウェブフォームがサポートしている唯一のメソッドであるためである. しかし, RESTfulなAPIを作るにあたり, 他のメソッドも有益である. 

技術的に言うと, どんな好きなメソッドでも用いることは可能である. しかし, HTTP仕様を遵守することが, 強く勧められる. これらの中で, もっとも一般的なものは次のようである:

### `GET`

Read-onlyリクエスト. サーバ側に全く変化が起こらないことを想定しており, `GET`リクエストを何回呼び出しても同じ結果になるはずで, 現在時刻や任意に割り振られた結果は除かれる.

### POST

一般的に変化を起こすリクエストである. `POST`リクエストはユーザによって2度提出されるべきではない. 一般的な例としては, ある銀行口座から別のところへ基金を送金することが挙げられる.

### PUT

サーバに新しいリソースを割り当てるか, 既存のものを交換する. このメソッドは何度呼ばれても安全である. 

### PATCH

サーバ上のリソースを部分的に更新する. リソースの1つまはたそれ以上のリソースをアップデートしたい場合, このメソッドが好まれる. 

### DELETE

文字通りである: サーバ上のリソースを削除する. 何度呼び出しても問題ない.

ある程度, これはHaskellの哲学に非常によく適合する: `GET`リクエストは純粋関数に類似しており, 副作用を持たない. 実践的には, `GET`関数はおそらく`IO`を行なっており, データベースから情報を取り出したり, ユーザの操作を記録したりしている. 

各リクエストメソッドに対しハンドラ関数を定義する構文についての詳細は, ルーティングとハンドラの章を参照しなさい. 

## 表現

次のようなHaskellデータ型と値があったとしよう.

``` haskell
data Person = Person { name :: String, age :: Int }
michael = Person "Michael" 25
```

このデータはHTMLでは次のように表せる:

``` html
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

または, JSONを用いて次のように表せる:

```
{"name":"Michael","age":25}
```

さらに, XMLを用いると, 

```
<person>
    <name>Michael</name>
    <age>25</age>
</person>
```

しばしば, ウェブアプリケーションは異なるURLを用いて, これらの表現を獲得する; おそらく, `/person/michael.html`, `/person/michael.json`などであろう. Yesodは各リソースにつき1つのURLというRESTful原則に従う. よって, Yesodにおいては, これら全ては`/person/michael`でアクセス可能である. 

そこで疑問となるのは, どの表現を用いるかを決定する方法である. 答えとなるのは, HTTP`ACccept`ヘッダである: それは, クライアントが期待しているコンテンツタイプの優先リストを与える. Yesodは関数のペアを持つことで, 直接ヘッダをパーズする詳細を取り除き, 代わりに, より高レベルの表現で扱えるようにする. 最後の文をコードでより具体化しよう.

``` haskell
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

`selectRep`関数は, "今まさに可能な表現を与えます"と言っている. 各`provideRep`の呼び出しは, 代わりの表現を与える. YesodはHaskellの型を用いて, 各表現におけるMIMEタイプを決定している. `shamlet`(すなわち, simple Hamlet)は`Html`値を作り, Yesodは関連するMIMEタイプは`text/html`であると決定できる. 同様に, `object`はJSON値を生成し, MIMEタイプは`application/json`であることを示唆する. `TypedContent`はYesodにより与えられたデータ型であり, 付属MIMEタイプを持つ生コンテンツのためにある. 

これを試すために, 次の異なる`curl`コマンドを走らせてみなさい.

``` bash
curl http://localhost:3000 --header "accept: application/json"
curl http://localhost:3000 --header "accept: text/html"
curl http://localhost:3000
```

Acceptヘッダ値に基づいてどのようにレスポンスが変化しただとうか. また, ヘッダを省略すると, HTMLレスポンスがデフォルトで表示される. ここでの規則は, もしAcceptヘッダが存在しなければ, 最初の表現が用いられることである. もし, Acceptヘッダが存在するが, 当てはまるものがなければ, "406アクセス不可能"レスポンスが返される.

デフォルトでは, Yesodは便利なミドルウェアを提供し, それは, クエリ文字列パラメータを用いてAcceptヘッダを設定する. これは, ブラウザからテストを行うことを容易にする. これを試すために, [http://localhost:3000/?`_`accept=application/json](http://localhost:3000/?_accept=application/json)を訪問せよ.

## JSONの利便性

今日においてJSONはウェブアプリケーションにおいて, 非常に一般的に用いられるフォーマットであるため, JSON表現を与えるような組込のヘルパ関数が存在する. これらは, 素晴らしい`aeson`ライブラリで構成されるため, そのライブラリの機能についての簡単な説明から始めよう. 

`aeson`は, コアのデータ型である`Value`を持っており, それは, あらゆる有効なJSON値を表す. それはまた, `ToJSON`と`FromJSON`の2つの型クラスを与え, それぞれJSON値へ, JSON値から, のマーシャリングを自動化する. 目的としては, 現在`ToJson`に興味がある. これまでに繰り返し出てきた`Person`データ型を用いて`ToJSON`インスタンスを作る簡単な例を見てみよう.

``` haskell
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

`aeson`については, これ以上深入りしないようにしよう. なぜなら, [Haddock文書](https://www.fpcomplete.com/haskell)がすでにライブラリを上手く紹介しているためである. これまでに示したことは, 便利関数を理解するためには十分である.

`Person`データ型と, 対応する値があり, 現在のページを表示するためにそれを用いたいとしよう. そのために, `returnJson`関数を用いる. 

``` haskell
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

`returnJson`は実際には, 自明な関数である; それは`return . toJSON`として実装される. しかし, それにより物事が少し便利になる. 同様に, JSON値を`selectRep`内部での表現として与えたい場合, `provideJson`を用いることができる. 

``` haskell
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

`provideJson`は同様に自明であり, この場合`provideRep . returnJson`で実装される. 

## 新しいデータ型

Haskellの`Show`インスタンスを用いて新しいデータフォーマットを思いついたとしよう. それを"Haskell Show"と呼び, `text/haskell-show`のMIMEタイプを与えよう. そして, この表現をウェブアプリケーションに含めたいとしよう. それはどのように行えばいいのか? 最初の試みとして, `TypedContent`データ型を直接用いよう. 

``` haskell
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

ここでは, 多少注意すべき重要な点がある.

- `toContent`関数を用いた. これは, いくつものデータ型を通信上に送信される準備ができた生データに変換するための, 型クラス関数である. 
この場合, `String`のインスタンスを用い, それはUTF8のエンコーディングを持つ. インスタンスを持つ一般的なデータ型としては, 他に`Text`, `ByteString`, `Html`, やaesonの`Value`などがある.

- `TypedContent`コンストラクタを直接用いている. これは, 2つの引数を取る: MIMEタイプと, 生コンテンツである. また, `ContentType`は, 単にstrict `ByteString`の型エイリアスである. 

これまではいいでしょうか, しかし, `getHomeR`の型注釈の情報量が少なすぎることは悩ましいことである. また, `getHomeR`の実装は, かなりボイラブレートに見える. これよりも, "Haskell Show"データ型
を表現するデータ型を持ち, このような値を作るための簡単な方法を与えたい. これを試してみよう.

``` haskell
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

ここでのトリックは, 2つの型クラスにある. 以前述べたように, `ToContent`はどのように値を生レスポンスに変換するかについて言っている. 今回の場合, もとの値を`show`することで, `String`を得, `String`を生コンテントに変換したい. しばしば, `ToContent`のインスタンスはこのように相互依存している. 

`ToTypedContent`は内部的にYesodが用いており, ハンドラ関数の結果に基づき呼び出される. ご覧の通り実装はかなりシンプルであり, 単にMIMEタイプを述べ, `toContent`を呼び出してるだけである. 

最後に, これをもう少し複雑にし, `selectRep`と上手く連携させてみよう.

``` haskell
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

ここで追加した重要な点は, `HasContentType`インスタンスである. これは, 冗長に見えるかもしれないが, 重要な役割を担う. 表現を作る前に, 起こりうる表現のMIME型を決定できる必要があるのである. `ToTypedContent`は具体的な値にのみ機能するため, 値を作る前には用いることができない. `getContentType`は代わりにプロキシー値を取り, 具体的なものを与えずとも型を示すことができる. 

<div class=yesod-book-notice>
もし, `HasContentType`インスタンスを持たない値に対し表現を与えたい場合, `provideRepType`を用いることができ, それは, MIME型が存在していることを明確に宣言することを必要とする.
<div>

## 他のリクエストヘッダ

利用可能なリクエストヘッダは数多く存在する. その内のいくつかは, サーバとクライアント間でのデータの移動にのみ関与し, アプリケーションには全く影響を与えない. 例えば, `Accept-Encoding`はサーバにどの圧縮スキームをクライアントが理解できるかを示し, `Host`はどの仮想ホストを用いるかについて示す.

他のヘッダはアプリケーションに関与するが, Yesodによって自動的に読まれる. 例えば, `Accept-Language`ヘッダは, どの人間の言語(英語, スペイン語, ドイツ語, スイス系ドイツ語)をクライアントが求めるかを特定する. このヘッダをどのように用いるかについては, i18nの章を見なさい.

## まとめ

YesodはRESTの次の原則に従う:

- 適切なリクエストメソッドを用いる.

- 各リソースは確実に1つのURLを持つ.

- 同じURLでデータを複数の方法で表現することを許容する.

- リクエストヘッダを調べ, クライアントが求めることについてのさらなる情報を得る.

これらは, Yesodを用いてウェブサイトを構築するだけでなく, APIを作ることも容易にする. 実際に, `selectRep`/`provideRep`のような技術を用いることで, ユーザフレンドリなHTMLページと, マシンフレンドリなJSONページを同じURLで用いることが可能になる.


