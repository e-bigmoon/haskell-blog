---
title: X-XSS-Protection レスポンスヘッダ
published: 2018/08/04
updated: 2020/03/03
---

この問題は [Set X-XSS-Protection to 1; mode=block. #1550](https://github.com/yesodweb/yesod/pull/1550) で解決されました。

## 関連情報

- [Setting X-XSS-Protection: 1 in response header by default? #1543](https://github.com/yesodweb/yesod/issues/1543)

## デフォルト

Yesod のデフォルトでは `X-XSS-Protection` レスポンスヘッダは特に設定されていません。

しかし、セキュリティ的には

```html
X-XSS-Protection: 1; mode=block
```

が設定されている方が望ましいです。これは、以下のようにすると良いです。

```hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
  yesodMiddleware handler = do
    addHeader "X-XSS-Protection" "1;mode=block"
    defaultYesodMiddleware handler

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  [whamlet|
    "test"
  |]

main :: IO ()
main = warp 3000 App
```

