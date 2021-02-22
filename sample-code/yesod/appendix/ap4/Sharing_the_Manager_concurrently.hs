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

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    runConcurrently $ sequenceA_ $ map Concurrently $ map ($ manager) [doSomething1, doSomething2]

doSomething1 :: Manager -> IO ()
doSomething1 manager = do
    let request = "http://httpbin.org/get"
    response <- httpLbs request manager
    withResponse request manager $ \response -> do
        valueEvent <- runConduit $ bodyReaderSource (responseBody response)
              .| eventConduit .| CL.consume
        print valueEvent

doSomething2 :: Manager -> IO ()
doSomething2 manager = do
    let request = "http://httpbin.org/get"
    response <- httpLbs request manager
    withResponse request manager $ \response -> do
        valueDoc <- runConduit $ bodyReaderSource (responseBody response)
              .| sinkDoc
        print valueDoc