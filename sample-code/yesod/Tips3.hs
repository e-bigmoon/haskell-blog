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
getHomeR = defaultLayout
  [whamlet|
    "test"
  |]

main :: IO ()
main = warp 3000 App
