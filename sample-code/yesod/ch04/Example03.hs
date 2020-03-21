#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package blaze-builder
    --package blaze-html
    --package http-types
    --package shakespeare
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Blaze.ByteString.Builder        (toByteString)
import           Control.Arrow                   (second)
import           Data.Text                       (Text, append, pack)
import           Data.Text.Encoding              (decodeUtf8)
import           Network.HTTP.Types              (renderQueryText)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Hamlet                     (HtmlUrl, hamlet)

data MyRoute = SomePage

render :: MyRoute -> [(Text, Text)] -> Text
render SomePage params = "/home" `append`
    decodeUtf8 (toByteString $ renderQueryText True (map (second Just) params))

main :: IO ()
main = do
    let currPage = 2 :: Int
    putStrLn $ renderHtml $ [hamlet|
<p>
    You are currently on page #{currPage}.
    <a href=@?{(SomePage, [("page", pack $ show $ currPage - 1)])}>Previous
    <a href=@?{(SomePage, [("page", pack $ show $ currPage + 1)])}>Next
|] render
