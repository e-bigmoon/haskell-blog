#!/usr/bin/env stack
-- stack script --resolver lts-17.3
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    let request = "http://httpbin.org/get"
            { proxy = Just $ Proxy "127.0.0.1" 3128
            }
    response <- httpLbs request manager

    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response