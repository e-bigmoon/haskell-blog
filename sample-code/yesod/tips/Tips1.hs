#!/usr/bin/env stack
-- stack script --resolver lts-15.1
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

  toWidget [julius|
    #{name}
  |]

main :: IO ()
main = warp 3000 App
