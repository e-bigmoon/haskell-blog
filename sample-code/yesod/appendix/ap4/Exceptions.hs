#!/usr/bin/env stack
-- stack script --resolver lts-17.3
{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception          (try)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
    eresponse <- try $ httpLBS "http://does-not-exist"

    case eresponse of
        Left e -> print (e :: HttpException)
        Right response -> L8.putStrLn $ getResponseBody response