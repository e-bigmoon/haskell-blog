#!/usr/bin/env stack
-- stack script --resolver lts-12.7
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import           Data.Time.LocalTime
import           Path

data Env = Env
  { envCounter  :: IORef Int
  , envLogPath  :: Path Rel File
  , envUserName :: String
  }

main :: IO ()
main = do
  counter <- newIORef 0
  let env = Env
        { envCounter  = counter
        , envLogPath  = $(mkRelFile "./output.log")
        , envUserName = "wado"
        }

  runReaderT (access >> access >> access) env

  n <- readIORef counter
  putStrLn $ mconcat ["counter = ", show n]

access :: ReaderT Env IO ()
access = ask >>= \Env {..} -> liftIO $ do
  modifyIORef envCounter (+ 1)
  now <- getZonedTime
  appendFile (toFilePath envLogPath)
    $ mconcat [envUserName, ": ", "access", " (", show now, ")", "\n"]
  return ()
