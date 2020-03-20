#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package yesod
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod

data App = App
mkYesod "App" [parseRoutes|
/ HomeR GET
|]

myLayout :: Widget -> Handler Html
myLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle pc}
                    <meta charset=utf-8>
                    <style>body { font-family: verdana }
                    ^{pageHead pc}
                <body>
                    <article>
                        ^{pageBody pc}
        |]

instance Yesod App where
    defaultLayout = myLayout

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>Hello World!
    |]

main :: IO ()
main = warp 3000 App
