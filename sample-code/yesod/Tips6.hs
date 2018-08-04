#!/usr/bin/env stack
-- stack script --resolver lts-12.4
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod
import Text.Julius

data App = App

mkYesod "App" [parseRoutes|
/check1 Check1R GET -- Safe
/check2 Check2R GET -- Safe
/check3 Check3R GET -- Safe
/check4 Check4R GET -- Unsafe
/check5 Check5R GET -- Unsafe
|]

instance Yesod App

-- http://localhost:3000/check1?p=<script>alert(document.cookie)</script>
getCheck1R :: Handler Html
getCheck1R = defaultLayout $ do
  mParam <- lookupGetParam "p"

  [whamlet|
    #{maybe "" id mParam}
  |]

-- http://localhost:3000/check2?p="></form>XSS"
getCheck2R :: Handler Html
getCheck2R = defaultLayout $ do
  mParam <- lookupGetParam "p"

  [whamlet|
    <form action="" method="POST">
      氏名<input name="name" value="#{maybe "" id mParam}"><br>
  |]

-- http://localhost:3000/check3?p=1+onmouseover%3dalert(document.cookie)
getCheck3R :: Handler Html
getCheck3R = defaultLayout $ do
  mParam <- lookupGetParam "p"

  [whamlet|
    <input type=text name=mail value=#{maybe "" id mParam}>
  |]

-- http://localhost:3000/check4?p=javascript:alert(document.cookie)
getCheck4R :: Handler Html
getCheck4R = defaultLayout $ do
  mParam <- lookupGetParam "p"

  [whamlet|
    <a href=#{maybe "" id mParam}>
      ブックマーク
  |]

-- http://localhost:3000/check5?p='),alert(document.cookie)//
getCheck5R :: Handler Html
getCheck5R = defaultLayout $ do
  mParam <- lookupGetParam "p"

  [whamlet|
    <body onload="init('#{maybe "" id mParam}')">
  |]

  toWidget [julius|
    function init(text) {
      // 適当な処理
    }
  |]

main :: IO ()
main = warp 3000 App
