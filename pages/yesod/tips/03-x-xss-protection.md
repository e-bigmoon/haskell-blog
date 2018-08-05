---
title: X-XSS-Protection レスポンスヘッダ
date: 2018/08/04
---

## 関連情報

- [Setting X-XSS-Protection: 1 in response header by default? #1543](https://github.com/yesodweb/yesod/issues/1543)

## デフォルト

Yesod のデフォルトでは `X-XSS-Protection` レスポンスヘッダは特に設定されていない。

しかし、セキュリティ的には

```html
X-XSS-Protection: 1; mode=block
```

が設定されている方が望ましいと思う。以下のようにすれば良い。

```hs
#!/usr/bin/env stack
-- stack script --resolver lts-12.4
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod

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

