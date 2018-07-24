#!/usr/bin/env stack
-- stack script --resolver lts-12.2

import System.IO

main :: IO ()
main = do
  h <- openFile "./shift-jis.txt" ReadMode
  cp932 <- mkTextEncoding "cp932"
  hSetEncoding h cp932
  content <- hGetContents h

  putStrLn content