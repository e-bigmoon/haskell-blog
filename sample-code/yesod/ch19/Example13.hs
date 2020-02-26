#!/usr/bin/env stack
-- stack script --resolver lts-14.19
{-# LANGUAGE QuasiQuotes #-}
import           Data.Text   (Text, pack)
import           Text.Julius (Javascript)
import           Text.Lucius (Css)
import           Yesod.Core

getHomeR :: LiteHandler Html
getHomeR = withUrlRenderer $
    [hamlet|
        $doctype 5
        <html>
            <head>
                <title>Hi There!
                <link rel=stylesheet href=/style.css>
                <script src=/script.js>
            <body>
                <h1>Hello World!
    |]

getStyleR :: LiteHandler Css
getStyleR = withUrlRenderer [lucius|h1 { color: red }|]

getScriptR :: LiteHandler Javascript
getScriptR = withUrlRenderer [julius|alert('Yay, Javascript works too!');|]

main :: IO ()
main = warp 3000 $ liteApp $ do
    dispatchTo getHomeR
    onStatic (pack "style.css") $ dispatchTo getStyleR
    onStatic (pack "script.js") $ dispatchTo getScriptR
