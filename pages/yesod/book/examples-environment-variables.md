---
title: 環境変数で設定しよう
published: 2018/03/18
updated: 2018/09/16
---

近頃の流行りとして [the twelve-factor app](https://12factor.net/config) が提唱するように、アプリケーションの全ての設定を、設定ファイルや (やってないとは思いますが) アプリケーションのソースコードにハードコーディングする代わりに、環境変数に保存しようという動きがあります。

Yesod の scaffolding は、どのアプリケーションでも必要だと思われる設定については、組み込みでサポートしています。よく使われる環境変数は、URLの生成方法に関する `APPROOT` 環境変数、リクエストを待ち受けるポート番号に関する `PORT` 環境変数、データベースの接続設定に関するものなどがあります。 (偶然にも、これは [the Keter deployment manager](https://github.com/snoyberg/keter) と相性が良いです)

実際のところ、このテクニックはとても簡単に実装できます。単に `main` 関数で環境変数を読み込むだけです。以下のコード例は、アプリケーションルートの設定に必要なちょっとした処理を含む具体例です。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text          (Text, pack)
import           System.Environment
import           Yesod

data App = App
    { myApproot      :: Text
    , welcomeMessage :: Text
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
    approot = ApprootMaster myApproot

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    App {..} <- getYesod
    setTitle "Environment variables"
    [whamlet|
        <p>Here's the welcome message: #{welcomeMessage}
        <p>
            <a href=@{HomeR}>And a link to: @{HomeR}
    |]

main :: IO ()
main = do
    myApproot <- fmap pack $ getEnv "APPROOT"
    welcomeMessage <- fmap pack $ getEnv "WELCOME_MESSAGE"
    warp 3000 App {..}
```

この例でトリッキーな部分は以下の2点です。

1. `getEnv` の戻り値は `String` 型の値なので、`pack` 関数を使って `Text` 型に変換する必要があります。
1. `aproot` の定義で `ApprootMaster` コンストラクタを利用していますが、これは "この関数をファウンデーション型に適用し、実際のアプリケーションルートを取得する" という意味になります。

## 本書のコード

- [Example01.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/examples/ex08/Example01.hs)