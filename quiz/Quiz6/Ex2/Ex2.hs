#!/usr/bin/env stack
{-
stack script --resolver lts-11.3
  --package conduit
  --package extra
  --package directory
-}

import Conduit

import System.Environment (getArgs)
import System.Directory (doesFileExist, getFileSize)
import Control.Monad.Extra (whenM)
import Control.Monad (when)

main :: IO ()
main = do
  arg <- getArgs
  when (length arg == 1) $ do
    (cnt, size) <-
      runConduitRes $ sourceDirectoryDeep True (head arg)
                   .| awaitForever getInfo
                   .| getZipSink ((,) <$> ZipSink lengthC <*> ZipSink sumC)
    putStrLn $ "総ファイル数: " ++ show cnt
    putStrLn $ "総ファイルサイズ: " ++ show size

getInfo :: MonadResource m => FilePath -> ConduitM FilePath Integer m ()
getInfo path =
  whenM (liftIO $ doesFileExist path) $ do
    size <- liftIO $ getFileSize path
    yield size