#!/usr/bin/env stack
-- stack script --resolver lts-17.3
{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent.Async  (Concurrently (..))
import qualified Data.ByteString.Char8     as S8
import qualified Data.ByteString.Lazy      as L
import           Data.Foldable             (sequenceA_)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import           Network.HTTP.Client 
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as S
import qualified Data.Conduit.List      as CL
import qualified Network.HTTP.Simple    as NTS
import           System.IO              (stdout)

import           Text.HTML.DOM
import           Data.Conduit ((.|), runConduit, runConduitRes)
import           Conduit (runResourceT)
import qualified Data.Conduit.Binary as CB
import           Network.HTTP.Client.Conduit (bodyReaderSource)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    doSomething manager
    
doSomething :: Manager -> IO ()
doSomething manager = do
    let request = "http://httpbin.org/get"

    response <- httpLbs request manager

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        valueEvent <- runConduit $ bodyReaderSource (responseBody response)
              .| eventConduit .| CL.consume
        valueDoc <- runConduit $ bodyReaderSource (responseBody response)
              .| sinkDoc
        print valueEvent
        print valueDoc