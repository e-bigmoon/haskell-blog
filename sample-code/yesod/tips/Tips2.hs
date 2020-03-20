#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package shakespeare
    --package yesod
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import Data.Maybe
import Text.Julius
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ OkR GET
/bad1 Bad1R GET
/bad2 Bad2R GET
|]

instance Yesod App where
--   yesodMiddleware handler = do
--     addHeader "Content-Security-Policy" "default-src 'self'"
--     defaultYesodMiddleware handler

getOkR :: Handler Html
getOkR = defaultLayout $ do
  mname <- lookupGetParam "name"

  [whamlet|
    $maybe name <- mname
      <img onload="init('#{renderJavascript $ toJavascript $ toJSON name}')" src="https://www.yesodweb.com/static/logo-home2-no-esod-smaller2.png">
  |]

  toWidget [julius|
    function init(text) {
      // Do whatever you want
    }
  |]

getBad1R :: Handler Html
getBad1R = defaultLayout $ do
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

getBad2R :: Handler Html
getBad2R = defaultLayout $ do
  mname <- lookupGetParam "name"

  [whamlet|
    <img onload="init('#{fromMaybe "" mname}')" src="https://www.yesodweb.com/static/logo-home2-no-esod-smaller2.png">
  |]

  toWidget [julius|
    function init(text) {
      // Do whatever you want
    }
  |]

main :: IO ()
main = warp 3000 App
