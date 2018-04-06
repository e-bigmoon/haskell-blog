#!/usr/bin/env stack
{-
stack script --resolver lts-11.3
  --package extra
  --package filepath
  --package directory
-}

import System.Environment (getArgs)
import System.Directory (listDirectory, doesFileExist, getFileSize)
import System.FilePath ((</>))
import Control.Monad.Extra (partitionM, ifM)
import Control.Monad (when)

main :: IO ()
main = do
  arg <- getArgs
  when (length arg == 1) $ do
    (cnt, size) <- recListDir $ head arg
    putStrLn $ "総ファイル数: " ++ show cnt
    putStrLn $ "総ファイルサイズ: " ++ show size

recListDir :: FilePath -> IO (Int, Integer)
recListDir fp = loop (0, 0) [fp]
  where
    loop summary [] = return summary
    loop (accCnt, accSize) (fp:fps) = do
      dirs <- listDirectory fp
      (files, childDirs) <- partitionM doesFileExist $ map (fp </>) dirs
      size <- sum <$> mapM getFileSize files
      let summary = (accCnt + length files, accSize + size)
      loop summary $ fps ++ childDirs