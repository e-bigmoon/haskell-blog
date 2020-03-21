---
title: TRACE メソッド
published: 2018/08/04
updated: 2020/03/03
---

## デフォルト

Yesod ではデフォルトで `TRACE` メソッドを禁止している。

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

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  [whamlet|
    "test"
  |]

main :: IO ()
main = warp 3000 App
```

以下のコマンドによって確認できる。

```shell
$ curl -I -X TRACE http://localhost:3000/
HTTP/1.1 405 Method Not Allowed
Date: Fri, 03 Aug 2018 16:11:51 GMT
Server: Warp/3.2.23 + Yesod/1.6.6 (core)
Content-Length: 167
Content-Type: text/html; charset=utf-8
Set-Cookie: _SESSION=IadXmuc8ESIVLlmLE0nyqgLxw/bsyRXo8hCM+LBqOXEm+B8gB/psgAS9KKeSbQlUevbQhyY6wOZURn4pZhU17luJDSC2E62sP41HLvdMCfhOXAa8nq4xSj7wZ2ufYGSSDd2U9UsbbA0=; Path=/; Expires=Fri, 03-Aug-2018 18:11:51 GMT; HttpOnly
Vary: Accept, Accept-Language
```

`HTTP/1.1 405 Method Not Allowed` とあるので禁止されていることがわかる。