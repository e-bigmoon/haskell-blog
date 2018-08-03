---
title: whamlet と julius
date: 2018/08/04
---

## whamlet の中で Text 型を展開する場合

```hs
#!/usr/bin/env stack
-- stack script --resolver lts-12.4
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import           Data.Text

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

この場合 `#{name}` を展開する時に [toHtml](https://www.stackage.org/haddock/lts-12.4/blaze-html-0.9.1.1/Text-Blaze-Html.html#v:toHtml) が呼ばれ `Text` 型が [Html](https://www.stackage.org/haddock/lts-12.4/blaze-html-0.9.1.1/Text-Blaze-Html.html#t:Html) 型に変換される。

## julius の中で Text 型を展開する場合

```hs
#!/usr/bin/env stack
-- stack script --resolver lts-12.4
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import           Data.Text

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

この場合 `#{name}` を展開する時に [toJavascript](https://www.stackage.org/haddock/lts-12.4/shakespeare-2.0.15/Text-Julius.html#v:toJavascript) が呼ばれるが、インスタンス定義に `Text` 型が含まれていないためエラーとなる。

```hs
/home/bm12/Desktop/repo/site-doc/haskell-blog/sample-code/yesod/Tips1.hs:26:20: error:
    • No instance for (shakespeare-2.0.15:Text.Julius.ToJavascript
                         Text)
        arising from a use of ‘shakespeare-2.0.15:Text.Julius.toJavascript’
```