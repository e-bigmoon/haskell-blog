---
title: Environment variables for configuration
date: 2018/03/18
---

## コンフィグレーションのための環境変数

最近, [the twelve-factor app](https://12factor.net/config) は, アプリケーションの全て
の設定について, 設定ファイルやアプリケーションのソースコードへのハードコーディングを避け, 環境変数に保存することを強く推奨している(そんなことはしていませんよね!).

Yesod の scaffolding は, これに対するサポート機能を組み込んでおり, よく使われるものでは, URLの生成方法に関する `APPROOT` 環境変数, リクエストを待ち受けるポート番号に関する `PORT` 環境変数, データベースの接続設定に関するものなどがある. (偶然にも, これは [the Keter deployment manager](https://github.com/snoyberg/keter) によく合致する.)

これを実施するためのテクニックは, かなり容易である: 単に `main` 関数において環境変数の探索を行うだけでよい. 以下の例では, アプリケーションルートを設定するために必要な少々の処理とともに, このテクニックを示す.

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

この例において, 唯一の巧妙な部分は以下にある.

1. `getEnv`によって返された `String` 値を, `pack` を用いて `Text` に変換する必要がある.
1. `aproot` のために `ApprootMaster` コンストラクタを用いており, これは "この関数をファウンデーションデータ型に適用し, 実際のアプリケーションルートを得る" ことを意味している.