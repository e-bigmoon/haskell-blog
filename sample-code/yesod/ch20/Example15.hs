#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package text
    --package yesod-core
-}
import           Data.Text  (pack)
import           Yesod.Core

getHomeR :: LiteHandler TypedContent
getHomeR = return $ TypedContent typeHtml $ toContent
    "<html><head><title>Hi There!</title>\
    \<link rel='stylesheet' href='/style.css'>\
    \<script src='/script.js'></script></head>\
    \<body><h1>Hello World!</h1></body></html>"

getStyleR :: LiteHandler TypedContent
getStyleR = return $ TypedContent typeCss $ toContent
    "h1 { color: red }"

getScriptR :: LiteHandler TypedContent
getScriptR = return $ TypedContent typeJavascript $ toContent
    "alert('Yay, Javascript works too!');"

main :: IO ()
main = warp 3000 $ liteApp $ do
    dispatchTo getHomeR
    onStatic (pack "style.css") $ dispatchTo getStyleR
    onStatic (pack "script.js") $ dispatchTo getScriptR