#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package blaze-html
    --package shakespeare
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Data.Text                       (Text)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Hamlet                     (HtmlUrl, hamlet)

data MyRoute = Home

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = "/home"

footer :: HtmlUrl MyRoute
footer = [hamlet|
<footer>
    Return to #
    <a href=@{Home}>Homepage
    .
|]

main :: IO ()
main = putStrLn $ renderHtml $ [hamlet|
<body>
    <p>This is my page.
    ^{footer}
|] render
