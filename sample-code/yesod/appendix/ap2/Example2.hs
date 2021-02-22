#!/usr/bin/env stack
-- stack script --resolver lts-17.4
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder           (fromByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import           Control.Concurrent.MVar
import           Data.Monoid                        ((<>))
import           Network.HTTP.Types                 (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp           (run)

application countRef _ respond = do
    modifyMVar countRef $ \count -> do
        let count' = count + 1
            msg = fromByteString "You are visitor number: " <>
                  fromShow count'
        responseReceived <- respond $ responseBuilder
            status200
            [("Content-Type", "text/plain")]
            msg
        return (count', responseReceived)

main = do
    visitorCount <- newMVar 0
    run 3000 $ application visitorCount