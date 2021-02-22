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
import           Network.Connection         (TLSSettings(TLSSettingsSimple))

import qualified System.IO as SI (readFile, stdout)
import qualified Data.ByteString        as S
import           Network.HTTP.Simple (httpSink, getResponseStatusCode)
import           Control.Monad.IO.Class (liftIO)
import           Text.HTML.DOM
import           Network.HTTP.Types
import qualified Text.XML  as TXML

main :: IO ()
main = do
    filecontent <- SI.readFile "urls.txt"
    let urls = map parseRequest_ $ lines filecontent
        settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    _ <- mapM (doSomething manager) urls
    runConcurrently $ sequenceA_ $ map Concurrently $ map ($ manager) [doSomething5] <*> urls
    pure ()

doSomething :: Manager -> Request -> IO ()
doSomething manager request = do
    response <- httpLbs request manager
    withResponse request manager $ \response -> do
        valueEvent <- runConduit $ bodyReaderSource (responseBody response)
              .| eventConduit .| CL.consume
        print valueEvent

doSomething2 :: Manager -> Request -> IO ()
doSomething2 manager request = do
    httpSink request $ \response -> do
    liftIO $ putStrLn
           $ "The status code was: "
          ++ show (getResponseStatusCode response)
    CL.mapM_ (S.hPut SI.stdout)

doSomething3 :: Manager -> Request -> IO ()
doSomething3 manager request = do
    httpSink request $ \response -> do
    liftIO $ putStrLn
           $ "The status code was: "
          ++ show (getResponseStatusCode response)
    CL.mapM_ (S.hPut SI.stdout)

doSomething4 :: Manager -> Request -> IO ()
doSomething4 manager request = do
    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)
        valueEvent <- runConduit $ bodyReaderSource (responseBody response)
              .| eventConduit .| CL.consume
        valueDoc <- runConduit $ bodyReaderSource (responseBody response)
              .| sinkDoc
        print valueEvent
        print valueDoc

doSomething5 :: Manager -> Request -> IO ()
doSomething5 manager request = do
    withResponse request manager $ \response -> do
        valueDoc <- runConduit $ bodyReaderSource (responseBody response)
              .| sinkDoc
        --print valueEvent
        let elems = TXML.elementAttributes $ TXML.documentRoot valueDoc
        print $ TXML.documentRoot valueDoc
