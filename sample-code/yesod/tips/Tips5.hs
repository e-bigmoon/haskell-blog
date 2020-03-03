#!/usr/bin/env stack
-- stack script --resolver lts-15.1
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import Web.Cookie
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  let moon =
        defaultSetCookie
          { setCookieName = "Moon"
          , setCookieValue = "huh!"
          -- , setCookieDomain = Just "yesod-bigmoon.com"
          }
      doesCookieRemainHttpOnly =
        defaultSetCookie
          { setCookieName   = "AmIHttpOnly"
          , setCookieValue  = "Hello"
          -- , setCookieDomain = Just "yesod-bigmoon.com"
          , setCookieHttpOnly = True
          }
  setCookie moon
  setCookie doesCookieRemainHttpOnly
  setCookie $ doesCookieRemainHttpOnly { setCookieValue = "How are you?" }

  [whamlet| <p>This is so beautiful day! |]

main :: IO ()
main = warp 3000 App
