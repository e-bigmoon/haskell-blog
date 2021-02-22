#!/usr/bin/env stack
-- stack script --resolver lts-17.4
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder (fromByteString)
import           Control.Concurrent       (threadDelay)
import           Network.HTTP.Types       (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)

application _ respond = respond $ responseStream status200 [("Content-Type", "text/plain")]
    $ \send flush -> do
        send $ fromByteString "Starting the response...\n"
        flush
        threadDelay 1000000
        send $ fromByteString "All done!\n"

main = run 3000 application