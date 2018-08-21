#!/usr/bin/env stack
-- stack script --resolver lts-12.7
{-# LANGUAGE TemplateHaskell #-}

import           Path
import           Path.IO

import           Control.Monad      (when)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  when (length args == 1) $ do
    let src  = $(mkRelDir "./src")
        dest = $(mkRelDir "./.backup")

    -- 安全にディレクトリを作成
    mapM_ ensureDir [src, dest]

    rawName <- parseRelFile $ head args
    fn <- (src </> rawName) -<.> "hs"

    writeFile (toFilePath fn) "main :: IO ()\nmain = undefined\n"

    -- ディレクトリを再帰的にコピー
    copyDirRecur' src dest
