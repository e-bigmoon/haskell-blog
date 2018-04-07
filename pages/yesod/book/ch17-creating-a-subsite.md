---
title: Creating a Subsite
date: 2018/04/07
---

## サブサイトを構築する

どれくらいのサイトが, 認証システムを与えているであろうか? もしくは, ある対象に対し, 作る-読む-更新する-削除する(CRUD)ための管理機能を与える必要があるであろうか? それは, ブログ, もしくはウィキであろうか.

ここでのテーマは, 多くのウェブサイトには複数のサイトにおいて再利用できる共通のコンポーネントが含まれるということである. しかし, 真に利便性に富んだモジュラー式コードを作ることは非常に難しいことである: コンポーネントは, たいてい複数のルートに対するルーティングシステムへのフックが必要であり, マスターサイトと外観情報を共有するための方法もまた必要であろう.

Yesodにおいては, そのための解決策はサブサイトである. サブサイトは, 容易にマスターサイトに挿入可能なルートとハンドラの集合である. 型クラスを用いることで, サブサイトはマスターサイトの機能を使えるようになり,
さらにデフォルトサイトのレイアウトにアクセスすることも可能になる.
そして, 型安全URLにより, マスターサイトからサブサイトへのリンクを容易になる.

### Hello World

おそらくサブサイトを書くにあたり最も技巧的な部分は, 最初の取っかかりである. 簡単なHello Worldのサブサイトから始めよう. サブサイトのデータ型を含めるために1つのモジュール, サブサイトのディスパッチコードのための別のモジュール, そして最後にサブサイトを用いるアプリケーションのためのモジュールを作ることが必要になる.

> データとディスパッチコードを分断する理由は, GHCステージ制約と呼ばれるもののためである. この必要性により, 小さなデモコードが少し冗長になるが, しかし現実的には, このように複数のモジュールに分割することは則るべきよい慣習である.

```haskell
-- @HelloSub/Data.hs
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module HelloSub.Data where

import           Yesod

-- Subsites have foundations just like master sites.
data HelloSub = HelloSub

-- We have a familiar analogue from mkYesod, with just one extra parameter.
-- We'll discuss that later.
mkYesodSubData "HelloSub" [parseRoutes|
/ SubHomeR GET
|]
```

```haskell
-- @HelloSub.hs
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module HelloSub
    ( module HelloSub.Data
    , module HelloSub
    ) where

import           HelloSub.Data
import           Yesod
import           Yesod.Core.Types (SubHandlerFor)

-- And we'll spell out the handler type signature.
getSubHomeR :: Yesod master => SubHandlerFor HelloSub master Html
getSubHomeR = liftHandler $ defaultLayout [whamlet|Welcome to the subsite!|]

instance Yesod master => YesodSubDispatch HelloSub master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesHelloSub)
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           HelloSub
import           Yesod

-- And let's create a master site that calls it.
data Master = Master
    { getHelloSub :: HelloSub
    }

mkYesod "Master" [parseRoutes|
/ HomeR GET
/subsite SubsiteR HelloSub getHelloSub
|]

instance Yesod Master

-- Spelling out type signature again.
getHomeR :: HandlerFor Master Html
getHomeR = defaultLayout
    [whamlet|
        <h1>Welcome to the homepage
        <p>
            Feel free to visit the #
            <a href=@{SubsiteR SubHomeR}>subsite
            \ as well.
    |]

main = warp 3000 $ Master HelloSub
```

この簡単なコードにより, サブサイトを構築する際に複雑な箇所の大部分が実際に示される. 通常のYesodアプリケーションと同様に, サブサイトにおけるあらゆるものはFoundation型を中心としており, 今回の場合, `HelloSub`がそれに対応する. そして, `mkYesodSubData`を用いて, サブサイトのルートデータ型と, それに紐づくパースやレンダー関数を作る.

ディスパッチ側においては, `SubHomeR`ルートのハンドラ関数を定義することから始めている. 次の関数の型注釈に注意を払ってもらいたい:

```haskell
getSubHomeR :: Yesod master => SubHandlerFor HelloSub master Html
```

これはサブサイトの心臓, 魂に当たる部分である. あらゆる操作は, この層構造モナドにあり, サブサイトがメインサイトをラップする形となっている. このモナド層構造を考慮すると, `liftHandler`を使うことになるのは驚くに値しないだろう. この場合, サブサイトはマスターサイトの`defaultLayout`関数を用いて, ウィジットをレンダリングしている.

`defaultLayout`関数は, Yesod型クラスの一部である. したがって, それを呼び出すために, `master`型引数は`Yesod`のインスタンスでなければならない. この方法の利点は, マスターサイトの`defaultLayout`に対するあらゆる変更が自動的にサブサイトに反映されることである.

サブサイトをマスターサイトのルート定義に埋め込む際に, 4つの情報を指定する必要がある: サブサイトのベースとして用いるルート(今回の場合, `/subsite`), サブサイトルートのコンストラクタ(`SubsiteR`), サブサイトのファウンデーションデータ型(`HelloSub`), そして, マスターファウンデーション値を取りサブサイトファウンデーション値を返す関数(`getHelloSub`)である.

`getHomeR`の定義において, どのようにルートコンストラクタが用いられるかを見た. ある意味では, `SubsiteR`は, サブサイトルートをマスターサイトルートの昇格させ, あらゆるマスターサイトテンプレートから安全にリンクすることが可能になる.