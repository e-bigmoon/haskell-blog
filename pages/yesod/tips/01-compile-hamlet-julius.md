---
title: whamlet と julius
published: 2018/08/04
updated: 2020/03/03
---

この問題は [ToJavascript instance of String and Text #227](https://github.com/yesodweb/shakespeare/pull/227) で解決されました。

## whamlet の中で Text 型を展開する場合

```hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import Data.Text
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  let name = "bigmoon" :: Text

  [whamlet|
    #{name}
  |]

main :: IO ()
main = warp 3000 App
```

この場合 `#{name}` を展開する時に [toHtml][1] が呼ばれ `Text` 型が [Html][2] 型に変換されます。

[1]: https://hackage.haskell.org/package/blaze-html-0.9.1.2/docs/Text-Blaze-Html.html#v:toHtml
[2]: https://hackage.haskell.org/package/blaze-html-0.9.1.2/docs/Text-Blaze-Html.html#t:Html

## julius の中で Text 型を展開する場合

```hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import ata.Text
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  let name = "bigmoon" :: Text

  toWidget [julius|
    #{name}
  |]

main :: IO ()
main = warp 3000 App
```

この場合 `#{name}` を展開する時に [toJavascript][3] が呼ばれますが、インスタンス定義に `Text` 型が含まれていないためエラーとなります。

[3]: https://hackage.haskell.org/package/shakespeare-2.0.24/docs/Text-Julius.html#v:toJavascript

```hs
/home/bm12/Desktop/repo/site-doc/haskell-blog/sample-code/yesod/Tips1.hs:26:20: error:
    • No instance for (shakespeare-2.0.15:Text.Julius.ToJavascript
                         Text)
        arising from a use of ‘shakespeare-2.0.15:Text.Julius.toJavascript’
```