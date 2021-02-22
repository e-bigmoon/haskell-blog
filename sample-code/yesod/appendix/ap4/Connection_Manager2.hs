#!/usr/bin/env stack
-- stack script --resolver lts-17.3
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple

main :: IO ()
main = do
    manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
    setGlobalManager manager

    let request = "http://httpbin.org/get"
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response