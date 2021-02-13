#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package yesod-core
-}
{-# LANGUAGE QuasiQuotes #-}
import           Yesod.Core

getHomeR :: LiteHandler Html
getHomeR = defaultLayout $ do
    setTitle $ toHtml "Hi There!"
    [whamlet|<h1>Hello World!|]
    toWidget [lucius|h1 { color: red }|]
    toWidget [julius|alert('Yay, Javascript works too!');|]

main :: IO ()
main = warp 3000 $ liteApp $ dispatchTo getHomeR