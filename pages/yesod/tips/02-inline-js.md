---
title: hamlet とインライン Javascript
date: 2018/08/04
---

## 関連情報

- [prevent XSS in event handlers #1547](https://github.com/yesodweb/yesod/issues/1547)
- [Updates to Julius Interpolation](https://www.yesodweb.com/blog/2012/11/updates-julius-interpolation)
- [Yesod には脆弱性があるのかな？](https://haskell.e-bigmoon.com/posts/2018/07-31-yesodsecurity.html)

## 脆弱性を含むコード

```hs
#!/usr/bin/env stack
-- stack script --resolver lts-12.4
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Maybe
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  mname <- lookupGetParam "name"

  [whamlet|
    $maybe name <- mname
      <img onload="init('#{name}')" src="https://www.yesodweb.com/static/logo-home2-no-esod-smaller2.png">
  |]

  toWidget [julius|
    function init(text) {
      // Do whatever you want
    }
  |]

main :: IO ()
main = warp 3000 App
```

例えば `http://localhost:3000/?name=%27),alert(1)//` にアクセスすると `alert` が実行されてしまう。

## 対策

### 方法1 インラインをやめる

`julius` に全部書く。

潜在的にではあるが、脆弱性は存在しているので、推奨はしない。(実装者によっては知らずに書いてしまうかもしれない)

### 方法2 Content Security Policy の設定

Content Security Policy を設定してインラインの JavaScript を禁止する。

```hs
instance Yesod App where
  yesodMiddleware handler = do
    addHeader "Content-Security-Policy" "default-src 'self'"
    defaultYesodMiddleware handler
```

### 方法3 明示的に javascript のエスケープを行う

rails の [escape_javascript](https://github.com/rails/rails/blob/master/actionview/lib/action_view/helpers/javascript_helper.rb#L27) を参考に自前でエスケープ処理を実装する。

今のところ、Yesod はそのような関数を提供していない。

ちなみに、以下のようにしても、やはり脆弱性は存在している。(内部関数の [string](https://www.stackage.org/haddock/lts-12.4/shakespeare-2.0.15/src/Text.Julius.html#string) ではエスケープ処理が不十分なため)

```hs
[whamlet|
  $maybe name <- mname
    <img onload="init('#{renderJavascript $ toJavascript $ toJSON name}')" src="https://www.yesodweb.com/static/logo-home2-no-esod-smaller2.png">
|]
```
