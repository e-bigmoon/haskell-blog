---
title: Yesod v1.6 のサブサイト
author: Shinya Yamaguchi
tags: bigmoon, yesod
---

## はじめに

今回は `Yesod` を使っているマニア向け情報です。

`Yesod` にはサブサイトという機能があり、Yesod Book の [Creating a Subsite](https://www.yesodweb.com/book/creating-a-subsite) で一通りの使い方が説明されています。

簡単に言えば、異なる `Yesod` アプリケーションで共通のコンポーネント (認証システム等) を利用するために使えるようです。

`scaffolded site` を使っている人は [yesod-static](https://www.stackage.org/lts-11.1/package/yesod-static-1.6.0) パッケージで提供されている `Static` 型をサブサイトとして利用していることでしょう。

`Yesod` のバージョンが `1.6` に上がった影響により `Yesod Book` の内容がそのままでは動かなくなっていたため、コードを修正して実際に動かしてみたいと思います。

<!--more-->

## 修正ポイント

- `HandlerT site IO a` のような形式は何も考えずに `HandlerFor site a` に書き換える
- サブサイトのハンドラ関数の型は `SubHandlerFor sub master a` という形式に書き換える
- `HandlerFor` から `SubHandlerFor` へ持ち上げる時は `liftHandler` を使う
- サブサイトの `YesodDispatch` 型クラスのインスタンスは `HandlerT` ではなく `instance Yesod master => YesodSubDispatch HelloSub master where` のように `Yesod` 型クラスのインスタンスを直接指定できるようになった

## 修正後のソースコード

ここでは公式サイトのコードと同様のディレクトリ構造及び、ファイル名とします。

```sh
$ tree .
.
├── HelloSub
│   └── Data.hs
├── HelloSub.hs
└── Main.hs

1 directory, 3 files
```

ファイルを分割しているのは `TH` の `GHC stage restriction` を回避するためです。

### HelloSub/Data.hs

```hs
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module HelloSub.Data where

import           Yesod

data HelloSub = HelloSub

mkYesodSubData "HelloSub" [parseRoutes|
/ SubHomeR GET
|]
```

サブサイトのファウンデーション型 `HelloSub` とルートを定義しています。

通常であれば `mkYesod` を利用しますが `mkYesodSubData` を利用している点がサブサイトの特徴です。

### HelloSub.hs

```hs
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
import           Yesod.Core.Types

getSubHomeR :: Yesod master => SubHandlerFor HelloSub master Html
getSubHomeR = liftHandler $ defaultLayout [whamlet|Welcome to the subsite!|]

instance Yesod master => YesodSubDispatch HelloSub master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesHelloSub)
```

サブサイトのハンドラと `dispatch` の定義です。

`resourcesHelloSub` は `HelloSub/Data.hs` で定義した `mkYesodSubData` でコンパイル時に自動生成されます。

### Main.hs

```hs
#!/usr/bin/env stack
-- stack --resolver lts-11.0 script --package yesod-core --package yesod

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           HelloSub
import           Yesod

data Master = Master
  { getHelloSub :: HelloSub
  }

mkYesod "Master" [parseRoutes|
/ HomeR GET
/subsite SubsiteR HelloSub getHelloSub
|]

instance Yesod Master

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

重要なポイントは以下の3点です。

- `Master` ファウンデーション型にサブサイトの型を含ませる (さらにアクセサ `getHelloSub` を定義)
- `/subsite SubsiteR HelloSub getHelloSub` という形式でサブサイトへのルートを定義
- サブサイトへの参照は `@{SubsiteR SubHomeR}` という形式の型安全URLとなる

## 実行

`stack interpreter` 形式で実行します。

```sh
$ chmod u+x Main.hs
$ ./Main.hs
19/Mar/2018:00:37:27 +0900 [Info#yesod-core] Application launched @(yesod-core-1.6.2-JztYji0NiLuH8rcbB3eMBP:Yesod.Core.Dispatch ./Yesod/Core/Dispatch.hs:167:11)
```

この状態で [http://localhost:3000/](http://localhost:3000/) にアクセスしてみましょう。以下のようなページが表示されるはずです。

![img](/images/2018/03-19/img01.png)

サブサイトにアクセスするとこんな感じです。

![img](/images/2018/03-19/img02.png)

## Scaffolded Site

ここではさらに一歩踏み込んで `Scaffolded Site` を利用する際、どのファイルに何を書いたら良いのか簡単に解説しておこうと思います。

`Scaffolded Site` は一番素朴な `yesod-simple` を利用することとします。

```sh
$ $ stack new example-subsite yesod-simple
$ cd example-subsite
```

サブサイトの内容は上記で定義した内容を再利用します。

### stack.yaml

```yaml
resolver: lts-11.0
packages:
- .
```

現状落ちてくるものは `yesod-1.6.0` ではないので、書き換えます。

### package.yaml

```yaml
dependencies:
- base
- classy-prelude-yesod
- yesod
- yesod-core
- yesod-static
- yesod-form
- classy-prelude
- classy-prelude-conduit
- bytestring
- text
- template-haskell
- shakespeare
- hjsmin
- monad-control
- wai-extra
- yaml
- http-conduit
- directory
- warp
- data-default
- aeson
- conduit
- monad-logger
- fast-logger
- wai-logger
- file-embed
- safe
- unordered-containers
- containers
- vector
- time
- case-insensitive
- wai
- foreign-store
```

バージョンを全部消しただけです。

### src/Foundation.hs

`App` データ型にサブサイトを含め、さらに `import` を追加します。

```hs
import HelloSub

data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static
  , appHttpManager :: Manager
  , appLogger      :: Logger
  , getHelloSub    :: HelloSub
  }
```

`yesod-1.6.0` した影響でコンパイルエラーが出るため、`shouldLog` をコメントアウトします。

```hs
    -- shouldLog app _source level =
    --     appShouldLogAll (appSettings app)
    --         || level == LevelWarn
    --         || level == LevelError
```

### config/route

サブサイトへのルートを追加します。

```txt
/subsite SubsiteR HelloSub getHelloSub
```

### src/Application.hs

以下の2点を追記します。

- `import HelloSub` を追加
- `makeFoundation` 関数に `let getHelloSub = HelloSub` を追記

```hs
import HelloSub

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  let getHelloSub = HelloSub

  appHttpManager <- newManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings then staticDevel else static)
    (appStaticDir appSettings)

  return App {..}
```

`App {..}` は `GHC` の `RecordWildCards` 言語拡張です。

```hs
data Example
  { a = a
  , b = b
  , c = c
  }
```

僕はあまり使いませんが、雰囲気はこんな感じです。

### 実行

```sh
$ stack build
$ stack exec example-subsite
```

[http://localhost:3000/subsite](http://localhost:3000/subsite) にアクセスすると次のような画面が現れます。

![img](/images/2018/03-19/img03.png)

## まとめ

`Yesod` を使っていてもサブサイトについて知らないという人は多いと思いますが、`Yesod` のアプリケーションが増えてきた時に使える、とても便利な仕組みだと思います。

サブサイトについて、もっと詳しく知りたい人は [yesod-static](https://www.stackage.org/lts-11.1/package/yesod-static-1.6.0) パッケージの実装を見てみると良いでしょう。

以上です。