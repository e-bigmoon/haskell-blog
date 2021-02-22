#!/usr/bin/env stack
-- stack script --resolver lts-17.3
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as S
import qualified Data.Conduit.List      as CL
import           Network.HTTP.Simple
import           System.IO              (stdout)

main :: IO ()
main = httpSink "http://httpbin.org/get" $ \response -> do
    liftIO $ putStrLn
           $ "The status code was: "
          ++ show (getResponseStatusCode response)

    CL.mapM_ (S.hPut stdout)