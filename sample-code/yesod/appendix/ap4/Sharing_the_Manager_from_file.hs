#!/usr/bin/env stack
-- stack script --resolver lts-17.3
{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent.Async  (Concurrently (..))
import           Data.Foldable             (sequenceA_)
import           Network.HTTP.Client 
import           Network.HTTP.Client.TLS

import qualified Data.Conduit.List      as CL

import           Text.HTML.DOM
import           Data.Conduit ((.|), runConduit, runConduitRes)
import           Conduit (runResourceT)
import qualified Data.Conduit.Binary as CB
import           Network.HTTP.Client.Conduit (bodyReaderSource)

import qualified System.IO as SI (readFile)

main :: IO ()
main = do
    filecontent <- SI.readFile "urls.txt"
    let urls = map parseRequest_ $ lines filecontent
    manager <- newManager tlsManagerSettings
    _ <- mapM (doSomething manager) urls
    pure ()

doSomething :: Manager -> Request -> IO ()
doSomething manager request = do
    response <- httpLbs request manager
    withResponse request manager $ \response -> do
        valueEvent <- runConduit $ bodyReaderSource (responseBody response)
              .| eventConduit .| CL.consume
        print valueEvent