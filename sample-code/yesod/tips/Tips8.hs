#!/usr/bin/env stack
-- stack script --resolver lts-15.1
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -ddump-splices #-}
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/check1 Check1R GET
|]

instance Yesod App

getCheck1R :: Handler Html
getCheck1R = defaultLayout $ do
  mParam <- lookupGetParam "p"

  [whamlet|
    #{maybe "" id mParam}
  |]

main :: IO ()
main = warp 3000 App
