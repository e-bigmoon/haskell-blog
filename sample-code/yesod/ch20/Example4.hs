#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package blaze-html
    --package http-types
    --package wai
    --package warp
-}
{-# LANGUAGE OverloadedStrings #-}
import           Network.HTTP.Types            (status200)
import           Network.Wai                   (Application, responseBuilder)
import           Network.Wai.Handler.Warp      (run)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import           Text.Blaze.Html5              (Html, docTypeHtml)
import qualified Text.Blaze.Html5              as H

main :: IO ()
main = run 3000 app

app :: Application
app _req sendResponse = sendResponse $ responseBuilder
    status200
    [("Content-Type", "text/html")] -- yay!
    (renderHtmlBuilder myPage)

myPage :: Html
myPage = docTypeHtml $ do
    H.head $ do
        H.title "Hello from blaze-html and Warp"
    H.body $ do
        H.h1 "Hello from blaze-html and Warp"