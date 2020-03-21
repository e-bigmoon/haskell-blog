#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package blaze-html
    --package text
    --package yesod-core
-}
import           Data.Text                   (pack)
import           Text.Blaze.Html5            (toValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Yesod.Core

getHomeR :: LiteHandler Html
getHomeR = return $ H.docTypeHtml $ do
  H.head $ do
    H.title $ toHtml "Hi There!"
    H.link ! A.rel (toValue "stylesheet") ! A.href (toValue "/style.css")
    H.script ! A.src (toValue "/script.js") $ return ()
  H.body $ do
    H.h1 $ toHtml "Hello World!"

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
