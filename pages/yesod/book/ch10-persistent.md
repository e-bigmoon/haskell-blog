---
title: Persistent
date: 2018/08/29
---

## Persistent

フォームはユーザとアプリケーションの境界を取り扱いました。
他に扱う必要のある境界はアプリケーションとストレージ間のレイヤです。
SQL データベース、YAML ファイル、バイナリ・ブロブのうち、SQL データベースとバイナリ・ブログのストレージレイヤはアプリケーションのデータ型を自然に理解できないので、何か手助けする必要があります。
Persistent は Haskell で型安全なデータストレージを統一的なデータストアインターフェースで扱うために Yesod で利用されます。

Haskell には多くの異なるデータベースバインディングがあります。
しかし、これらの大部分はスキーマに対する知識をほとんど持っていないため、有益な静的保証を与えることができません。
そのため、データベース依存の API やデータ型をプログラマに強要します。

Haskeller の中にはより革新的な方法を試した人もいます。
強く型付けされた Haskell のデータを容易に保存できる Haskell に特化したデータストアを作ることです。
この選択は特定の領域においては役に立ちますが、ライブラリによって提供されたストレージ技術に縛られるため、他の言語とうまく調和できません。

一方で Persistent は Haskell データ型の型安全性を維持しつつ、データストレージの利用目的ごとに高度にチューニングされた既存のデータベースを選択し、他のプログラム言語と同時に利用でき、安全で生産的なクエリインターフェースの利用が可能になります。

Persistent は型安全、正確さ、宣言的構文をガイドライン原理とします。
ほかの素晴らしい特徴を以下に挙げます。

- データベースに依存しません。PostgreSQL、 SQLite、 MySQL、 MongoDB と実験的に Redis をサポートしています
- 柔軟なデータモデリング。Persistent はモデル関係を定義し、それらを型安全な方法で利用できる。デフォルトの型安全 persistent API は join 操作をサポートしないことで、より広範な数のストレージレイヤを使えるようにしています。Join や他の SQL 特有の機能は、生の SQL レイヤを利用することで達成できます (かなり型安全性に乏しいが)。 付加的なライブラリである [Esqueleto](https://github.com/bitemyapp/esqueleto) は Persistent データモデルの最上層に構築され、型安全な join や SQL の機能を追加しています。
- 開発環境のデータベースマイグレーションが自動化できるので、開発スピードがアップします。

Persistent は Yesod と上手く機能しますが、単独でもスタンドアローンなライブラリとしてかなり役立ちます。
この章の大部分は単体の Persistent について説明しています。

## Synopsis

``` haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing

    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId

    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity BlogPost])

    john <- get johnId
    liftIO $ print (john :: Maybe Person)

    delete janeId
    deleteWhere [BlogPostAuthorId ==. johnId]
```

上のスニペットの型注釈はコードのコンパイルには不要ですが、それぞれの値の型がわかるように明示的に追加しました。

## Solving the boundary issue

SQL データベースに人間の情報を保存するとき、テーブルは次のようになるでしょう。

```sql
CREATE TABLE person(id SERIAL PRIMARY KEY, name VARCHAR NOT NULL, age INTEGER)
```

PostgreSQL のようなデータベースを使っているなら、 age フィールドにテキストを絶対に格納しないことが保証されています。 (SQLite では同じことが言えませんが、今はそのことは忘れましょう)
このデータベーステーブルを対応させるために、次のような Haskell データ型を作るでしょう。

```haskell
data Person = Person
    { personName :: Text
    , personAge  :: Int
    }
```

全ては型安全のように見えます。
データベーススキーマは Haskell のデータ型と一致し、データベースによって不適切なデータは決してデータストアの中に入り込まないことが保証され、全てのものが素晴らしいように見えます。
それは、以下の出来事が起きなければの話です。

- データベースからデータを読み込むと、データベースレイヤは型付けされていない形式でデータを返します
- 32歳より年上の人を全員探すとき、偶然 SQL 文 に "thirtytwo" と書いてしまったとしましょう。何が起こると思いますか？これは正しくコンパイルされますが、実行時まで問題があることに気づけないでしょう
- アルファベット順で初めの10人を探したいとしましょう。SQL でタイピングミスをするまでは大丈夫です。しかし、これも実行時まで見つけれらないでしょう

動的言語はこれらの問題を解決するために単体テストを行いますが、間違う可能性のある全てのものに対しテストケースが網羅されていることを確実にする必要があります。
しかし、すでに気づいていると思いますが、それは Yesod のアプローチ法とは合いません。
私たちは可能な限り Haskell の強い型付けの利点を使って、データストレージが例外を投げないようにしたいのです。

従って、次のような疑問が残ります。
どうすれば Haskell の型システムを使ってそれを解決することができるんだろう？

### Types

ルーティングの場合のように、型安全なデータアクセスについて本質的に難しいことは何もありません。
それは単に多くの単調で間違いやすいボイラプレートコードが必要になるだけです。
これはたいてい、型システムを使う良いタイミングであることを示しています。
また、退屈な仕事を避けるために Template Haskell を少しだけ使います。

`PersistValue` は Persistent の基本的な構成要素です。
これはデータベースと値を送受信するための直和型で表現されるデータ構造です。

定義は以下のようになっています。

``` haskell
data PersistValue
    = PersistText Text
    | PersistByteString ByteString
    | PersistInt64 Int64
    | PersistDouble Double
    | PersistRational Rational
    | PersistBool Bool
    | PersistDay Day
    | PersistTimeOfDay TimeOfDay
    | PersistUTCTime UTCTime
    | PersistNull
    | PersistList [PersistValue]
    | PersistMap [(Text, PersistValue)]
    | PersistObjectId ByteString
    -- ^ Intended especially for MongoDB backend
    | PersistDbSpecific ByteString
    -- ^ Using 'PersistDbSpecific' allows you to use types
    -- specific to a particular backend
```

それぞれの Persistent のバックエンドは、関連する値をデータベースが理解出来る何らかの値へ翻訳する方法を知らなければなりません。
しかし、データ全てを単にこれらの基本的な型で表現しなくてはならないことはばかげています。
次のレイヤは `PersistField` 型クラスで、これは任意の Haskell データ型と `PersistValue` との相互変換について定義します。
`PersistField` は SQL データベースのカラムに対応します。
先ほどの "人間" の例で具体例を確認すると、名前と年齢はそれぞれ `PersistField` になります。

最後の型クラスはユーザー側のコードを結びつけるための `PersistEntity` です。
`PersistEntity` のインスタンスは SQL データベースのテーブルに対応します。
この型クラスは多数の関数や関連する型を定義しています。
Persistent と SQL の対応関係を整理すると次のようにまとめられます。

SQL | Persistent
----|:------------:
DataTypes (VARCHAR, INTEGERなど) | PersistValue
Column | PersistField
Table | PersistEntity

### Code Generation

PersistEntity インスタンスが Haskell データ型と正確に一致することを確実にするために Persistent は両方の責任を持ちます。
エンティティを一度だけ定義すればよいのですから、これは DRY (自身を繰り返さない) の観点から素晴らしいものです。
簡単な例を見てみましょう。

```haskell
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)

mkPersist sqlSettings [persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]
```

(ルートを定義するときのように) テンプレート Haskell と準クォートの組み合わせを利用します。
`persistLowerCase` は空白が意味を持つ構文をエンティティ定義のリストに変換する準クォートです。
"LowerCase" は生成されるテーブル名の形式を意味します。
今回の場合は `SomeTable` のようなエンティティは SQL テーブルでは `some_table` となります。
`persistentFileWith` を使えば、他のファイルにエンティティを定義することもできます。
`mkPersist` はエンティティリストを取り、次の宣言を定義します。

- 各エンティティにつき、ひとつの Haskell データ型
- 定義された各データ型に対する `PersistEntity` インスタンス

先ほどの例では、次のようなコードを生成します。

```haskell
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings, GADTs #-}
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Control.Applicative

data Person = Person
    { personName :: !String
    , personAge :: !Int
    }
  deriving Show

type PersonId = Key Person

instance PersistEntity Person where
    newtype Key Person = PersonKey (BackendKey SqlBackend)
        deriving (PersistField, Show, Eq, Read, Ord)
    -- A Generalized Algebraic Datatype (GADT).
    -- This gives us a type-safe approach to matching fields with
    -- their datatypes.
    data EntityField Person typ where
        PersonId   :: EntityField Person PersonId
        PersonName :: EntityField Person String
        PersonAge  :: EntityField Person Int

    data Unique Person
    type PersistEntityBackend Person = SqlBackend

    toPersistFields (Person name age) =
        [ SomePersistField name
        , SomePersistField age
        ]

    fromPersistValues [nameValue, ageValue] = Person
        <$> fromPersistValue nameValue
        <*> fromPersistValue ageValue
    fromPersistValues _ = Left "Invalid fromPersistValues input"

    -- Information on each field, used internally to generate SQL statements
    persistFieldDef PersonId = FieldDef
        (HaskellName "Id")
        (DBName "id")
        (FTTypeCon Nothing "PersonId")
        SqlInt64
        []
        True
        NoReference
    persistFieldDef PersonName = FieldDef
        (HaskellName "name")
        (DBName "name")
        (FTTypeCon Nothing "String")
        SqlString
        []
        True
        NoReference
    persistFieldDef PersonAge = FieldDef
        (HaskellName "age")
        (DBName "age")
        (FTTypeCon Nothing "Int")
        SqlInt64
        []
        True
        NoReference
```

予想通り `Person` データ型は, もともとのテンプレート Haskell 版で与えた定義とかなり一致しています。
Generalised Algebraic Deta Type (GADT) はフィールド毎に戻り値の型が異なるコンストラクタを与えます。
この GADT はエンティティの型とフィールドの型の両方をエンコードしています。
そのコンストラクタは Persistent 全体を通して利用されます。
例えば、フィルタを適用する際にフィルタする値の型がフィールドに一致するか確認する場合です。
また、このエンティティのデータベースプライマリキーに対応する associated newtype 宣言があります。

Haskell の型と同様に生成された `Person` データ型を利用することができ、それを他の Persistent 関数に受け渡すことができます。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.IO.Unlift
import           Data.Text
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Conduit

share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
|]

runSqlite' :: (MonadUnliftIO m) => Text -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runSqlite' = runSqlite

main :: IO ()
main = runSqlite' ":memory:" $ do
    michaelId <- insert $ Person "Michael" $ Just 26
    michael <- get michaelId
    liftIO $ print michael
```

> このコードはコンパイルできますが、テーブルが存在しないため実行時エラーとなります。以下でその問題についての対処法を説明します。

標準的なデータベース接続コードから始めましょう。
この場合、単一コネクション関数を利用しました。
Persistent はコネクションプール関数を備え付けているため、一般的に製品版ではそちらを利用します。

この例では2つの関数が存在します。
`insert` はデータベースに新しいレコードを生成し、その ID を返します。
Persistent の他の全てのものと同様に ID は型安全です。
これらの ID がどのように機能するかについては後で詳しく説明します。
なので `insert $ Person "Michael" 26` を呼び出すと `PersonId` 型の値が返ってきます。

次は `Id` を使ってデータベースから値を読み込もうとする `get` 関数について説明します。
Persistent では、間違ったテーブルに対してキーを使ってしまう可能性はありません。
(`House`のような) 異なるエンティティを `PersonId` を使って読みこもうとしても、絶対にコンパイルできないからです。

### PersistStore

前の例で、一つだけ説明していない関数が残っていました。
`runSqlite` は正確には何を行って、どんなモナドでデータベースアクションが実行されているのでしょうか？

全てのデータベース操作は `PersistStore` インスタンスのパラメータが必要です。
その名前からわかるように、各データストア (PostgreSQL、SQLite、MongoDB) は `PersistStore` のインスタンスです。

この型クラスで `PersistValue` からデータベース特有の値へのあらゆる変換が起こり、SQL のクエリ生成などが始まります。

> 想像できるように `PersistStore` は安全でしっかりと型付けされたインターフェースを外部の世界に提供します。そこには、間違いが起こってしまうような多くのデータベース操作が存在していますが、このコードを1つの場所で自動的にかつ徹底的にテストすることで、エラーに陥りやすいコードを集約し、できる限りバグが出ないようにできます。

`runsqlite` は与えられたコネクション文字列を使って単一コネクションを作ります。
今回の例ではインメモリデータベースの利用を表す `:memory` を使いました。
あらゆる SQL バックエンドは `SqlBackend` を `PersistStore` の共通のインスタンスとして使い、それぞれの定義を与えます。
そして `runSqlite` は `runReaderT` を使って、アクションに対し `SqlBackend` 値を環境パラメータとして扱います。

> 実際には、`PersistUpdate` と `PersistQuery` という別の型クラスもあります。それぞれの型クラスが異なる機能を提供することで、 Persistent で利用可能な全ての高レベルな機能を提供できない (Redisのような) 単純なデータストアをバックエンドに利用することができる。

ただ1つ注意すべき重要なことは `runSqlite` の1回の呼び出しの内部で起こるあらゆることが、単一トランザクションで実行されるということです。
これには2つの重要な意味を含んでいます。

- 多くのデータベースで、トランザクションをコミットすることはコストのかかる動作です。複数のステップを単一のトランザクションに入れることでコードを劇的に高速化できます
- `runSqlite` の単一呼び出しどこかで例外が投げられた場合、全てのアクションは (バックエンドがロールバックサポートを持つことを想定しています) ロールバックされます

> これは、実際には最初に思っていたよりも、とても大きなインパクトを与えます。リダイレクトのような Yesod における多くのショートカット関数は例外を用いて実装されています。もし、 Persistent ブロック内部からこのような呼び出しを行えば、それはトランザクション全体をロールバックしてしまうでしょう

## Migration

ごめんなさい、これまでのところ少し嘘を付いていました。
少し前の例は実際には機能しません。
それを実行しようとすると、欠損テーブルに関するエラーが発生します。

SQL データベースの苦痛の1つにスキーマの変更管理があります。
明示的に指定すれば、ユーザの代わりに Persistent にこの作業を任せることもできます。
何が起こるか見てみましょう。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.IO.Unlift
import           Data.Text
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Conduit

share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)
    michaelId <- insert $ Person "Michael" $ Just 26
    michael <- get michaelId
    liftIO $ print michael
```

先ほどのコードの1箇所をほんの少しだけ変更すれば Persistent は自動的に `Person` テーブルを作成します。
`runMigration` と `migrate` を分割することで複数のテーブルを同時マイグレーションが可能になります。

> 注意: 自動データベースマイグレーション機能は開発環境にだけ利用することをおすすめします。プロダクション環境のデータベーススキーマが自動的に変更されてしまうのはめちゃめちゃヤバイと思います。自動化されたマイグレーションは開発スピードを速めるためにあります。そのため、プロダクション環境にデプロイする前に行う人手によるレビューやテストの代わりになるものではありません。

これは、少しのエンティティであれば気になりませんが、多くのエンティティを扱うようになるとすぐに退屈なものになります。
この退屈な作業を自分で行う代わりに Persistent には `mkMigrate` という補助関数があります。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
Car
    color String
    make String
    model String
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do runMigration migrateAll
```

`mkMigrate` はテンプレート Haskell 関数であり `persist` ブロックで定義した全てのエンティティに対し `migrate` を自動的に呼び出します。
`share` は `persist` ブロックからの情報を各テンプレート Haskell 関数に渡し結果を連結だけの補助関数です。

Persistent はかなり保守的なルールに従って、マイグレーションを実行します。
データベースからテーブル情報を読み込むことから始まり、SQL データ型を全て定義することで完了します。
そして、それをコードの中で与えられるエンティティ定義と比較します。
以下の場合は自動的にスキーマを入れ替えます。

- フィールドのデータ型の変更。しかし、データが翻訳できなかった場合は、おそらくデータベースはこの変更に反対します
- フィールドの追加。しかし、フィールドが null でなく、初期値も設定されていない (初期値に関しては後で説明します)、そして、データベースに既存のデータが存在する場合, データベースはフィールドの追加を許可しません。
- フィールドの not null から null への変更。逆の場合は Persistent はデータベースの承認次第で変換を行おうとします
- 新規エンティティの追加

しかし、中には Persistent が自動的に処理できない場合があります。

- フィールドまたはエンティティの名前変更。Persistent は "name" が "fullName" に変更されたことを知る方法がありません。それが見ているのは "name" と呼ばれる古いフィールドか "fullName" と呼ばれる新しいフィールドです。
- フィールドの削除。これはデータの消失につながるため Persistent はデフォルトでこのアクションを拒否しています(オススメしませんが `runMigration` の代わりに `runMigrationUnsafe` を用いることで削除を強制できます)

`runMigration` は実行中のマイグレーションを `stderr` (`runMigrationSilent` を使えば表示しないことも可能) に表示します。
そして、可能であれば常に `ALTER TABLE` を使います。
しかし SQlite では `ALTER TABLE` は非常に限定的な能力しかないため、 Persistent ではあるテーブルから別のテーブルへコピーする方法を仕方なく採用しました。

最後に、マイグレーションを実行する代わりに Persistent でどんなマイグレーションが必要なのかヒントが欲しい時は `printMigration` 関数を利用しましょう。
この関数は `runMigrate` が実行するマイグレーションを表示します。
これは Persistent が実行できないようなマイグレーションを行ったり、任意のSQLをマイグレーションに追加したり、どんなマイグレーションが起こったかについてログを出力するために役立つかもしれません。

## Uniqueness

エンティティ内でフィールドを宣言する時に、一意性の制約を追加できます。
良くある例は、ユーザ名の一意性です。

```haskell
User
    username Text
    UniqueUsername username
```

各フィールド名は小文字で始まる必要がありますが、 Haskell のデータコンストラクタとして表現されるため、一意性の制約は大文字で始まる必要があります。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    age Int
    PersonName firstName lastName
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
    insert $ Person "Michael" "Snoyman" 26
    michael <- getBy $ PersonName "Michael" "Snoyman"
    liftIO $ print michael
```

フィールドのユニークな組み合わせを宣言するために、新たに1行追加します。
行が大文字で始まるため Persistent はユニークコンストラクタを定義していることを知っています。
続く各単語はこのエンティティにおけるフィールドでなければなりません。

一意性における主な制約は non-null フィールドにのみ適用可能ということです。
その理由は、SQLは標準で一意性が `NULL` に対してどのように適用されるかについて曖昧だからです。 (例えば `NULL=NULL` は真なのか偽なのか)。
その曖昧さに加え、大部分の SQL は実際に Haskell のデータ型の期待するものに反する規則を実装しています (例えば PostgreSQL では `NULL=NULL` は偽ですが、一方で Haskell で `Nothing==Nothing` は真です)。

さらに、一意性の制約はデータベースレベルでのデータの一貫性について素晴らしい保証を与えるとともに、上記の例の `getBy` のように Haskell のコードで何らかの特別なクエリを実行するためにも利用されます。
これは `Unique` 関連型として扱い、上記の例においては、次のような新しいコンストラクタが出てくるでしょう。

``` haskell
PersonName :: String -> String -> Unique Person
```

> MongoDB のバックエンドでは一意性の制約は作られないため、ユニークインデックスをフィールドに指定しなければなりません。

## Queries

目的に応じてデータベースに問い合わせをするための異なる方法が存在します。
数値 ID に基づいて問い合わせをするものもあれば、フィルタを行うものもあります。
クエリは返す結果の数においても異なります。
検索の中には (検索キーが一意であれば) 1つしか結果を返さないものもあれば、多くの結果を返すものも存在します。

そのため Persistent にはいくつかの異なるクエリ関数が用意されています。
いつも通り、できる限り多くの不変量を型の中にエンコードします。
例えば 0 または 1 の結果しか返さないクエリは `Maybe` 型を利用し、一方で多くの結果を返すクエリはリストとして返します。

### Fetching by ID

Persistent で実行できる最も簡単なクエリは ID に基づいて取得を行うことです。
この値は存在しない場合があるため、その戻り値は `Maybe` 型となっています。 

``` haskell
personId <- insert $ Person "Michael" "Snoyman" 26
maybePerson <- get personId
case maybePerson of
    Nothing -> liftIO $ putStrLn "Just kidding, not really there"
    Just person -> liftIO $ print person
```

これは `/person/5` のような URL を与えるサイトにおいて非常に役立ちます。
しかし、このような場合たいてい `Maybe` についてはあまり気にせずただ値だけが欲しいため、見つからなかった場合は 404 メッセージを返します。
`get404` 関数 (yesod-persistent パッケージにあります) はまさにそのように動作する関数です。
詳しくは Yesod と統合するときに説明します。

### Fetching by unique constraint

`getBy` は以下の点を除けば `get` と同じです。

1. 一意性の制約を取ります。つまり、ID の代わりに `Unique` 値を取ります
1. 値の代わりに `Entity` を返します。`Entity` はデータベース ID と値の組み合わせです

```haskell
personId <- insert $ Person "Michael" "Snoyman" 26
maybePerson <- getBy $ PersonName "Michael" "Snoyman"
case maybePerson of
    Nothing -> liftIO $ putStrLn "Just kidding, not really there"
    Just (Entity personId person) -> liftIO $ print person
```

`get404` と 同様に `getBy404` 関数もあります。

### Select functions

多くの場合、もっと強力なクエリが必要となるでしょう。
例えば、ある年齢以上の人、青い色で利用可能な全ての車、電子メールアドレスの登録が無いユーザ全体などを見つけたい場合です。
このためには選択関数が1つ必要です。

あらゆる選択関数はわずかに結果が異なりますが、似たようなインターフェースとなっています。

関数 | 戻り値
-----|:------:
selectSource | `Sorce` はデータベースから全ての ID と値を含みます。この関数でストリーミングコードを記述できます。`Source` はデータのストリームであり `conduit` パッケージの一部です。この関数を使う前に [School of Haskell conduit tutorial](https://www.schoolofhaskell.com/user/snoyberg/library-documentation/conduit-overview) を読むことをオススメします
selectList | データベースから全ての ID と値をリストとして返します
selectFirst | 利用できれば、データベースから最初の ID と値を取り出すだけです
selectKeys | `Source` として値を含まないキーだけを返します

`selectList` は最も一般的に利用されるため特に詳しく説明します。
`selectList` が理解できれば、他の関数を理解することも簡単でしょう。

`selectList` は `Filter` のリストと `SelectOpt` のリストの2つの引数を取ります。`Filter` は "等号, より小さい、含む" などの演算に基づいて結果を制限します。 `SelectOpt` は並び替え、出力行数の制限、結果のオフセットといった3つの異なる機能を制御します。

> ウェブアプリケーションの効率的なページネーションが可能になるため、制限とオフセットの組み合わせは非常に重要です。

フィルタリングの例に進みんで、それを分析してみましょう。

```haskell
people <- selectList [PersonAge >. 25, PersonAge <=. 30] []
liftIO $ print people
```

単純な例なので、３つの点だけ説明すれば大丈夫でしょう。

1. `PersonAge` は関連ファントム型のコンストラクタです。この単語は強そうに聞こえますが、重要なことは一意的に "person" テーブルの "age" カラムが特定され、 `age` フィールドが `Int` であることが分かるという点です (そこがファントムの部分です)
1. Persistent フィルタリング演算子は何種類か用意されています。記法はとても覚えやすく、想定する演算子の最後にピリオドを置くだけです。3つだけ、わかりづらい演算子があるので別途説明します
1. フィルタのリストは `AND` で結合されるので、上記の例は "年齢が25より大きく、30以下" を意味します。`OR` で結合する方法は後で説明します

びっくりするような名前の演算子が "ノットイコール" です。
`/=` はアップデート (後で説明する、分割とセット) に使われるため `!=` を利用します。
コンパイラが指摘してくれるため、もし間違って使ってしまった場合でも大丈夫です。

"含む"、"含まない" 演算子もあまり直感的ではなく、それぞれ `<-.` と `/<-.` になります。 (どちらもピリオドで終わります)

そして `OR` については `||.` 演算子を利用します。
以下に例を示します。

```haskell
people <- selectList
    (       [PersonAge >. 25, PersonAge <=. 30]
        ||. [PersonFirstName /<-. ["Adam", "Bonny"]]
        ||. ([PersonAge ==. 50] ||. [PersonAge ==. 60])
    )
    []
liftIO $ print people
```

この (完全に意味のない) 例は "年齢が 26-30 の人" または "名前が Adam でも Bonny でもない人" または "年齢が50" または "60の人" を見つけます。

### SelectOpt

これまで見てきた全 `selectList` の利用方法では全て第2引数を空リストにしていました。
それはオプションを指定しないため、データベースが望むようにソートし、どの結果も飛ばさずに、全ての結果を返すということを意味します。
`SelectOpt` はそれら全てを変更するために4つのコンストラクタがあります。

#### Asc

昇順で与えられたカラムを並び替えます。
これは `PersonAge` のようにフィルタリングと同じファントム型を使います。

#### Desc

`Asc` と同じですが、降順です。

#### LimitTo

`Int` 引数を取ります。
指定された数まで結果を返します。

#### OffsetBy

`Int` 引数を取ります。
指定された数だけ結果をスキップします

次のコードは結果をページに分割する関数を定義します。
それは18歳以上で、彼らを (最も年を取っている人を最初とし) 年齢で並び替えた結果を返します。
年齢が同じ人については、名前、苗字のアルファベット順で並び替えます。

```haskell
resultsForPage pageNumber = do
    let resultsPerPage = 10
    selectList
        [ PersonAge >=. 18
        ]
        [ Desc PersonAge
        , Asc PersonLastName
        , Asc PersonFirstName
        , LimitTo resultsPerPage
        , OffsetBy $ (pageNumber - 1) * resultsPerPage
        ]
```

## Manipulation

クエリを学習したことで、やっと本章の半分が終わりました。
これから、データベースに値を追加したり、既存のデータを変更したりする方法を見ていきましょう。

### Insert

データベースのデータを操作できることは良いことです。
しかし、データを操作するためには何をすれば良いでしょうか？
答えは `insert` 関数です。
この関数はデータベースに値を与えると ID が返ってきます。

ここで Persistent の理念について少し触れておきましょう。
多くの ORM 解決策ではデータ格納に使われるデータ型が不明瞭です。
データを取得・変更するために定義されたインターフェースを経由する必要があります。
これは Persistent には当てはまらず、全てに通常の代数的データ型が使われています。
そのため、パターンマッチ、カリー化など、様々な素晴らしい利点を得られます。

しかし、できないこともあります。
例えば Haskell のレコードが更新されるごとにデータベースの値を自動的に更新する方法がないことです。
もちろん Haskell における純粋性と不変性の通常の考え方からすれば、このことはいずれにせよあまり重要でないので、気にすることではありません。

しかし、新規利用者が悩まされる1つの問題があります。
なぜ ID と値が完全に独立しているのでしょうか？
ID を値に埋め込むことは、かなり論理的に見えます。
つまり、なぜ

```haskell
data Person = Person { name :: String }
```

とする代わりに

```haskell
data Person = Person { personId :: PersonId, name :: String }
```

としないのだろうか？ということです。

こうすると、すぐに1つの問題が発生します。
どのように `insert` を行えば良いのでしょうか？
Person が ID を含む必要があるのであれば ID を挿入によって得ることができますが、挿入は Person が必要となり、終了しないループに陥ります。
これは `undefined` で解決できますが、それは単に面倒を起こしているだけです。

少しだけ安全にしたら良いと言うかも知れません。

``` haskell
data Person = Person { personId :: Maybe PersonId, name :: String }
```

`insert $ Person undefined "Michael"` よりも `insert $ Person Nothing "Michael"` の方が確実に好まれます。
そして、今の型はずっと単純になったでしょう？
例えば `selectList` は `[Entity SqlPersist Person]` の代わりに、シンプルな `[Person]` を返します。

問題は "醜さ" が信じがたい程に役に立つということです。
`Entity Person` とすることで、型レベルで見ればデータベースに存在する値を扱っていることが明確になります。
`PersonId` を必要とする他のページへのリンクを作りたいとしましょう (後から議論するように、これは良くあることです)。
`Entity Person` 形式は情報への明確なアクセスが可能になります。
`PersonId` を `Maybe` 型を使って `Person` に埋め込むと、コンパイル時チェックのエラー証明の代わりに、実行時における余分な `Just` チェックが生じてしまいます。

最後に ID を値に埋め込むと意味論的なミスマッチが発生します。
`Person` は値です。 (Haskellの文脈においては) 全てのフィールドが同じであれば2人は同一人物です。
ID を値に挿入することでもはや人ではなくデータベースの行について語ることになるのです。
等しさはもはや本当の等しさではなくなってしまいます。
つまり "同じ人" ではなく "同じデータ" となります。

要するに ID を独立して持つことには面倒な点もありますが、全体としてはそれは正しい方法なので、大きな枠組みの中においてはより良いバグの少ないコードにつながります。

### Update

このまま更新についても考えてみましょう。
最も簡単な更新の方法は次のようになります。

``` haskell
let michael = Person "Michael" 26
    michaelAfterBirthday = michael { personAge = 27 }
```

これは更新しているように見えますが、実際のところは古い `Person` 値に基づいて新しい値を作っているだけです。
更新と言うのは Haskell の値の変更のことを言っているのではありません。(Haskell のデータはイミュータブルなので、もちろんそうするべきでもありません)

代わりに、テーブルの行を変更する方法を見ていきます。
最も簡単な方法は `update` 関数を利用する方法です。

``` haskell
personId <- insert $ Person "Michael" "Snoyman" 26
update personId [PersonAge =. 27]
```

`update` は ID と `Update` のリストを引数に取ります。
最も簡単な更新は代入ですが、例として最もふさわしいとは限りません。
年齢を1だけ増やしたい場合に、現年齢が不明の場合はどうでしょうか？
Persistent では次のようにします。

```haskell
haveBirthday personId = update personId [PersonAge +=. 1]
```

また、`+=.`、 `-=.`、 `*=.`、 `/=.` など、全ての基本的な数学的演算子があります。
当然これらは単一のレコードを更新するために役立ち、さらに、適切な ACID を保証するために重要です。
`Person` を取り出し、年齢を1つ増やし、新しい値を更新することを考えてみてください。
同時にデータベース上で動作している2つのスレッド/プロセスがあったとしたら、苦痛の世界に直面するでしょう (ヒント:レースコンディション)。

たまには多くの行を1度に更新したい (例えば、全ての従業員の給料を5%上げたい) 時があるでしょう。
そういう場合に便利な `updateWhere` 関数はフィルターのリストと適用する更新のリストを取ります。

```haskell
updateWhere [PersonFirstName ==. "Michael"] [PersonAge *=. 2] -- it's been a long day
```

同じように、データベースの値を全く違う値に置き換えたい時があるかもしれません。
そのためには (驚きの) `replace` 関数を使います。

```haskell
personId <- insert $ Person "Michael" "Snoyman" 26
replace personId $ Person "John" "Doe" 20
```

### Delete

痛ましいことですが、データを削除しなければならない時があります。
そのために、3つの関数が用意されています。

#### delete

ID でデータを削除します。

#### deleteBy

一意性の制約を使って、データ削除します。

#### deleteWhere

フィルターを使って、データを削除します。

```haskell
personId <- insert $ Person "Michael" "Snoyman" 26
delete personId
deleteBy $ PersonName "Michael" "Snoyman"
deleteWhere [PersonFirstName ==. "Michael"]
```

テーブルの全てのデータを削除するためにも `deleteWhere` を使います。
どのテーブルを対象にするか GHC にヒントを与えるだけです。

```haskell
deleteWhere ([] :: [Filter Person])
```

## Attributes

ここまでの例では `persistLowerCase` の基本的な構文を見てきました。
エンティティ名の行に続いて、インデントされた2ワード (フィールド名とデータ型) の行が続きます。
Persistent はこれ以外にも、初めの2ワードの後に任意の属性リストを書けばより多くのことを制御できます。

例えば (オプションの) 年齢と彼/彼女がシステムに追加された時間のタイムスタンプで `Person` エンティティを作りたいとしましょう。
既にデータベースに存在するエンティティについてはタイムスタンプは単に現在の日時を利用する。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Control.Monad.IO.Class

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    created UTCTime default=CURRENT_TIME
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    time <- liftIO getCurrentTime
    runMigration migrateAll
    insert $ Person "Michael" (Just 26) time
    insert $ Person "Greg" Nothing time
    return ()
```

`Maybe` はフィールドがオプションになるような、単一ワード属性です。
これは Haskell では `Maybe` でラップされていることを意味し、SQL では NULL を許可します。

`default` 属性はバックエンドに依存し、データベースで理解可能なあらゆる構文が使えます。
今回の例では、データベースに内臓されている `CURRENT_TIME` 関数を利用しました。
次に、好きなプログラミング言語のフィールドを追加してみましょう。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    created UTCTime default=CURRENT_TIME
    language String default='Haskell'
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
```

> `default` 属性は Haskell のコード自体にはに全く影響を与えません。全ての値を埋めたいだけです。これは、データベーススキーマと自動マイグレーションにのみ影響します。

データベースが正しく解釈できるように文字列は単一のクォートで囲む必要があります。
また、 Parsistent は空白を含む場合はダブルクォートを使います。
よって、デフォルトの故郷を El Salvador にしたい時は次のようになります。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    created UTCTime default=CURRENT_TIME
    language String default='Haskell'
    country String "default='El Salvador'"
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
```

属性で使える1つのトリックは SQL テーブルとカラムで利用される名前を指定できることです。
これは、既存のデータベースを利用する場合に役立ちます。

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person sql=the-person-table id=numeric_id
    firstName String sql=first_name
    lastName String sql=fldLastName
    age Int "sql=The Age of the Person"
    PersonName firstName lastName
    deriving Show
|]
```

エンティティ定義構文には様々な多くの機能があります。
最新のリストは [Persistent documentation](https://github.com/yesodweb/persistent/blob/master/docs/Persistent-entity-syntax.md) を参照してください。

## Relations

Persistent はサポートしている非 SQL データベースと一貫する方法でデータ型の間における参照が可能になります。
今回は ID を関連するエンティティに埋め込んでみましょう。
つまり、1人の人が多くの車を持っているという関係です。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    deriving Show
Car
    ownerId PersonId
    name String
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
    bruce <- insert $ Person "Bruce Wayne"
    insert $ Car bruce "Bat Mobile"
    insert $ Car bruce "Porsche"
    -- this could go on a while
    cars <- selectList [CarOwnerId ==. bruce] []
    liftIO $ print cars
```

このテクニックを使えば1対多の関係を定義できます。
多対多の関係を定義するためにはオリジナルのテーブルのそれぞれと1対多の関係を持つ join エンティティが必要となります。
これらに一意性の制約をつけることも良い案です。
例えば、どの人がどの店で買い物をしたかを追跡したい状況をモデル化する場合です。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
Store
    name String
PersonStore
    personId PersonId
    storeId StoreId
    UniquePersonStore personId storeId
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    bruce <- insert $ Person "Bruce Wayne"
    michael <- insert $ Person "Michael"

    target <- insert $ Store "Target"
    gucci <- insert $ Store "Gucci"
    sevenEleven <- insert $ Store "7-11"

    insert $ PersonStore bruce gucci
    insert $ PersonStore bruce sevenEleven

    insert $ PersonStore michael target
    insert $ PersonStore michael sevenEleven

    return ()
```

## Closer look at types

これまでの所 `Person` や `PersonId` について、実際にそれらが何であるかについて説明せずに話を進めてきました。
最も単純な場合 (SQL に限定したシステム) の `PersonId` は `type PersonId = Int64` です。
しかし、これは `PersonId` を型レベルにおいて `Person` エンティティに結合させるものが何もないことを意味します。
そのため、`PersonId` を使って `Car` を偶然取得できるかも知れません。
この関係性をモデル化するためにファントム型使って以下のように定義しました。

```haskell
newtype Key entity = Key Int64
type PersonId = Key Person
```

これは、実際のところ本当に良く機能しますが、ID に `Int64` を使わないバックエンドが出てくると話は違います。
この問題は理論的な話ではなく、実際に MongoDB は `Int64` の代わりに `ByteSting` を使います。
そのため、 `Int64` と `ByteString` を含むことができるキーの値が必要になります。
このような時は、直和型が最も役立ちます。

```haskell
data Key entity = KeyInt Int64 | KeyByteString ByteString
```

しかしこれは、またもや問題を引き起こします。
次に、タイムスタンプを使うバックエンドがあったとすれば `Key` に別のコンストラクタを追加する必要があります。
こればしばらく続くでしょう。
ただ、幸運なのは任意データを表すための直和型 `PersistValue` が既に存在していることです。

```haskell
newtype Key entity = Key PersistValue
```

これは (おおよそ) Persistent がバージョン2.0まで使っていた実装です。
しかし、これはデータを捨ててしまうという別の問題を引き起こします。
例えば SQL データベースを扱っている時 (デフォルトが使われると想定すれば) キーの型が `Int64` になることを知っています。
しかし、型レベルでこのことを断言することはできません。
従って Persistent 2.0からは `PersistentEntity` クラス内部で関連データ型として定義します。

```haskell
class PersistEntity record where
    data Key record
    ...
```

SQLバックエンドでカスタムキー型を利用していないとすれば、 `Int64` が newtype でラップされ、 `toSqlKey` / `fromSqlKey` 関数は型安全な変換を代わりに行ってくれます。
また、MongoDB では `ByteString` でラップされます。

### More complicated, more generic

デファルトでは Persistent は特定のデータベースのバックエンドで機能するためにデータ型をハードコードしています。
`sqlSettings` を使えば `SqlBackend` 型になります。
しかし、複数のバックエンドを利用する Persistent コードを書くために `sqlSettings` を `sqlSettings { mpsGeneric = True }` とすることで、よりジェネリックな型が利用可能になります。

なぜこれが必要か理解するために、関係を考えてみましょう。
ブログとブログポストを表すために、以下のようなエンティティの定義を使います。

```haskell
Blog
    title Text
Post
    title Text
    blogId BlogId
```

`BlogId` が `Key Blog` の型シノニムだということはわかっていますが、どうやって `Key Blog` は定義されるのでしょうか？
`Int64` は MongoDB では機能しないため利用できません。
また `ByteString` も SQL データベースで機能しないため使えません。

これを許容するため `mpsGeneric` が `True` にセットされれば、結果のデータ型に使用しているデータベースのバックエンドを示すパラメータを含むようになり、キーは正しくエンコードされ、以下のようになるでしょう。

```haskell
data BlogGeneric backend = Blog { blogTitle :: Text }
data PostGeneric backend = Post
    { postTitle  :: Text
    , postBlogId :: Key (BlogGeneric backend)
    }
```

まだ、コンストラクタやレコードが短い名前であることに注目してください。
最後に、通常のコードにわかりやすいインターフェースを与えるために型シノニムを定義します。

```haskell
type Blog   = BlogGeneric SqlBackend
type BlogId = Key Blog
type Post   = PostGeneric SqlBackend
type PostId = Key Post
```

`SqlBackend` は Persistent のどこにもハードコードされていないと言われてしまうかもしれませんが、`mkPersist` に渡している `sqlSettings` パラメータは `SqlBackend` を使うように指示するものです。
Mongo のコードでは、代わりに `mongoSettings` を利用します。

これは、水面下では非常に複雑ですが、その複雑さはユーザには滅多に現れません。
この章を振り返ってみても、一度も `Key` や `Generic` などを直接扱う必要はありませんでした。
複雑さが現れる最も一般的な場所はコンパイラのエラーメッセージです。
そのため、これが存在することを覚えておくことは重要ですが、それが毎日のように影響することはないでしょう。

## Custom Fields

ある時、データストアにカスタムフィールドを定義したくなるでしょう。
最も良くあるケースは雇用状態の列挙などです。
この目的のために Persistent はテンプレート Haskell の補助関数を提供しています。

```haskell
-- @Employment.hs
{-# LANGUAGE TemplateHaskell #-}
module Employment where

import Database.Persist.TH

data Employment = Employed | Unemployed | Retired
    deriving (Show, Read, Eq)
derivePersistField "Employment"
```

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist.Sqlite
import Database.Persist.TH
import Employment

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    employment Employment
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    insert $ Person "Bruce Wayne" Retired
    insert $ Person "Peter Parker" Unemployed
    insert $ Person "Michael" Employed

    return ()
```

`derivePersistField` は文字列フィールドを使ってデータベースにデータを格納します。
そして、データ型の `Show` と `Read` インスタンスを使って相互変換を行います。
これは整数値を使って格納する方法と比較して効率的でないかも知れませんが、ずっと将来性のあるものです。
将来的にコンストラクタを付け加えたとしても、データは有効であり続けるからです。

> 今回は定義を2つの別々のモジュールに分割しました。これは GHC のステージ制限を回避するために必要なことです。これは、本質的には、テンプレート Haskell で生成されたコードは多くの場合で作られたモジュールと同じ場所では利用できないことを意味しています。

## Persistent: Raw SQL

Persistent パッケージはデータストアへの型安全なインターフェースを提供しています。
インターフェースは SQL にだけ存在する機能に頼ったりせず、極力バックエンドに依存しないようにしています。
経験上、やりたいことの 95% は高レベルのインターフェースを利用して楽に実行できます。 (実際に私のウェブサイトの大部分はもっぱら高レベルのインターフェースを利用しています)

しかし、残りの5%でバックエンド特有の機能を利用する場合があるでしょう。
過去に利用した1つの機能は全文テキスト検索です。
この場合 SQL の "LIKE" 演算子を利用したくなりますが、 Persistent ではモデル化されていません。
名前が "Snoyman" の人を全員取り出しレコードを表示する例をみてみましょう。

> 実際のところ Persistent 0.6 に追加された機能を使えば LIKE 演算子を直接通常の構文で表現でき、バックエンド特有の演算子が利用可能になります。しかし, これはかなり良い例なので、気にせず進めましょう。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import Database.Persist.TH
import Data.Text (Text)
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import qualified Data.Conduit.List as CL

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name Text
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
    insert $ Person "Michael Snoyman"
    insert $ Person "Miriam Snoyman"
    insert $ Person "Eliezer Snoyman"
    insert $ Person "Gavriella Snoyman"
    insert $ Person "Greg Weber"
    insert $ Person "Rick Richardson"

    -- Persistent does not provide the LIKE keyword, but we'd like to get the
    -- whole Snoyman family...
    let sql = "SELECT name FROM Person WHERE name LIKE '%Snoyman'"
    rawQuery sql [] $$ CL.mapM_ (liftIO . print)
```

自動的なデータ変換を可能にする高レベルサポートもあります。
詳細については Haddock API ドキュメントを参照してください。

## Integration with Yesod

Persistent の力を理解してもらえたと思いますが、どのように Yesod アプリケーションに統合されるのでしょうか？
scaffolding を使っていれば、大部分の作業は既に完了していると思いますが、いつも通り手動で全てのものを作成し、水面下でどのように動いているかを理解します。

yesod-persistent パッケージは Persistent と Yesod を接着するために `YesodPersist` 型クラスを提供し、`runDB` メソッドを通して DB への標準的なアクセスが可能となります。
具体列を見てみましょう。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

-- Define our entities as usual
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    age Int
    deriving Show
|]

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
data PersistTest = PersistTest ConnectionPool

-- We'll create a single route, to access a person. It's a very common
-- occurrence to use an Id type in routes.
mkYesod "PersistTest" [parseRoutes|
/ HomeR GET
/person/#PersonId PersonR GET
|]

-- Nothing special here
instance Yesod PersistTest

-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.
instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlBackend

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

-- List all people in the database
getHomeR :: Handler Html
getHomeR = do
    people <- runDB $ selectList [] [Asc PersonAge]
    defaultLayout
        [whamlet|
            <ul>
                $forall Entity personid person <- people
                    <li>
                        <a href=@{PersonR personid}>#{personFirstName person}
        |]

-- We'll just return the show value of a person, or a 404 if the Person doesn't
-- exist.
getPersonR :: PersonId -> Handler String
getPersonR personId = do
    person <- runDB $ get404 personId
    return $ show person

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Person "Michael" "Snoyman" 26
    warp 3000 $ PersistTest pool
```

この例には一般的に利用する際の2つの重要な点があります。
`runDB` は `Handler` の内部で DB アクションを実行するために使われます。
`runDB` の内部で `insert` や `selectList` のような、これまで話した関数をどれでもつかうことができます。

> `runDB` の型は `YesodDB site a → HandlerT site IO a` です。`YesodDB` は次のように定義されます。

``` haskell
type YesodDB site = ReaderT (YesodPersistBackend site) (HandlerT site IO)
```

> `YesodPersistBackend` 関連型の上に構築されるため、現在のサイトに基づいて適切なデータベースのバックエンドを利用します。

他の新しい機能は `get404` です。これは `get` と全く同じように振舞いますが、結果が見つからない場合に `Nothing` を返す代わりに404エラーメッセージページを返します。
`getPersonR` 関数は `get404` が値に基づきレスポンスを返すという、実際の Yesod アプリケーションで良く見る手法です。

## More complex SQL

Persistent はバックエンドに依存しません。
この方法のメリットは、コードが異なるバックエンド型の間を自由に行き来できることです。
デメリットはバックエンド特有の機能を見失うことです。
最も大きなものは SQL の join サポートです。

幸運にも Felipe Lessa と Chris Allen のおかげで、この問題は解決できる。
[Esqueleto](https://github.com/bitemyapp/esqueleto) ライブラリは既存の Persistent 基盤を使って、型安全な SQL クエリの記述をサポートしてくれます。
Esqueleto の Haddock は利用方法の良い導入となるでしょう。
また Persistent の概念を多く使っているため Persistent の知識の大部分が Esqueleto でも利用できるでしょう。

Esqueleto 利用する簡単な例は SQL Join の章を参照してください。

## Something besides SQLite

この章の例を単純にするため SQLite バックエンドを利用してきました。
最後に、一番はじめの例を PostgreSQL でも動くように書き直したコードを以下に示します。

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

connStr = "host=localhost dbname=test user=test password=test port=5432"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        johnId <- insert $ Person "John Doe" $ Just 35
        janeId <- insert $ Person "Jane Doe" Nothing

        insert $ BlogPost "My fr1st p0st" johnId
        insert $ BlogPost "One more for good measure" johnId

        oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
        liftIO $ print (oneJohnPost :: [Entity BlogPost])

        john <- get johnId
        liftIO $ print (john :: Maybe Person)

        delete janeId
        deleteWhere [BlogPostAuthorId ==. johnId]
```

## Summary

Persistent は Haskell の型安全性をデータアクセスレイヤに適用します。
間違いやすい型のないデータアクセスのためのボイラプレートコードを手動で書く代わりに、Persistent はその作業を自動化してくれます。

ゴールは、ほとんどの場合に必要な全てのものを提供することです。
より強力なものが必要な時は Persistent が表面下のデータストアへの直接的なアクセスを提供し、書きたければ5通りの join を書くことができます。

Persistent は一般的な Yesod のワークフローに直接的に統合します。
`yesod-persistent` のような補助パッケージだけが素晴らしいレイヤを提供するだけでなく、`yesod-form` や `yesod-auth` のようなパッケージもまた Persistent の機能を向上させてくれます。

エンティティの構文やデータベースコネクション等に関するより詳しい情報は https://github.com/yesodweb/persistent/tree/master/docs を参照してください。