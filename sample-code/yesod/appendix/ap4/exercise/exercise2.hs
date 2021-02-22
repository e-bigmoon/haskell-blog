#!/usr/bin/env stack
-- stack script --resolver lts-17.3
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.HTTP.Simple
import           Network.HTTP.Conduit       (mkManagerSettings)
import           Network.Connection         (TLSSettings(TLSSettingsSimple))

main :: IO ()
main = do
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings

    let request = setRequestManager manager "https://httpbin.org/get"
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response