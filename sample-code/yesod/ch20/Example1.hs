#!/usr/bin/env stack
-- stack script --resolver lts-14.27
{-# LANGUAGE OverloadedStrings #-}
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, responseLBS)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3000 app

app :: Application
app _req sendResponse = sendResponse $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello Warp!"