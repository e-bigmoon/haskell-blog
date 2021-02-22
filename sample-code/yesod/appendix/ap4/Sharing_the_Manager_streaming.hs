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

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    runConcurrently $ sequenceA_ $ replicate 16
                    $ Concurrently $ doSomething manager

doSomething :: Manager -> IO ()
doSomething manager = do
    let request = "https://httpbin.org/get"

    response <- httpLbs request manager

    let msg = encodeUtf8 $ T.pack $ concat
            [ "Got a message with status code "
            , show $ statusCode $ responseStatus response
            , " with response body length "
            , show $ L.length $ responseBody response
            , "\n"
            ]

    -- Using bytestring-based output to avoid interleaving of string-based
    -- output
    S8.putStr msg

    let requestWithManager = NTS.setRequestManager manager request
    let requestSecure = NTS.setRequestSecure True requestWithManager
    NTS.httpSink requestSecure $ \response -> do
      liftIO $ putStrLn
             $ "The status code was: "
             ++ show (NTS.getResponseStatusCode response)

      CL.mapM_ (S.hPut stdout)