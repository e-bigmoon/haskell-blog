#!/usr/bin/env stack
-- stack script --resolver lts-17.4
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.HTTP.Types (status200)

application _ respond = respond $ responseLBS status200 [("Content-Type", "text/plain")]
                       "Hello World"

main = run 3000 $ gzip def application