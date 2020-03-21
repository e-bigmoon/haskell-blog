#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package blaze-builder
    --package http-types
    --package transformers
    --package wai
    --package warp
-}
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder           (Builder, fromByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import           Control.Concurrent                 (threadDelay)
import           Control.Monad                      (forM_)
import           Control.Monad.Trans.Class          (lift)
import           Network.HTTP.Types                 (status200)
import           Network.Wai                        (Application, responseStream)
import           Network.Wai.Handler.Warp           (run)

main :: IO ()
main = run 3000 app

app :: Application
app _req sendResponse = sendResponse $ responseStream
    status200
    [("Content-Type", "text/html")]
    myStream

myStream :: (Builder -> IO ()) -> IO () -> IO ()
myStream send flush = do
  send $ fromByteString "Starting streaming response.\n"
  send $ fromByteString "Performing some I/O.\n"
  flush
  -- pretend we're performing some I/O
  threadDelay 1000000
  send $ fromByteString "I/O performed, here are some results.\n"
  forM_ [1..50 :: Int] $ \i -> do
    send $ fromByteString "Got the value: " <>
           fromShow i <>
           fromByteString "\n"