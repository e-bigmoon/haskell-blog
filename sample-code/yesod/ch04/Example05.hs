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

data MyRoute = Home | Time | Stylesheet

render :: MyRoute -> [(Text, Text)] -> Text
render Home _       = "/home"
render Time _       = "/time"
render Stylesheet _ = "/style.css"

template :: Text -> HtmlUrl MyRoute
template title = [hamlet|
$doctype 5
<html>
    <head>
        <title>#{title}
        <link rel=stylesheet href=@{Stylesheet}>
    <body>
        <h1>#{title}
|]

main :: IO ()
main = putStrLn $ renderHtml $ template "My Title" render
