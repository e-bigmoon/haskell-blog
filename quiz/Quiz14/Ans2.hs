#!/usr/bin/env stack
-- stack script --resolver lts-12.2

import           System.IO.Extra

main :: IO ()
main = do
  cp932 <- mkTextEncoding "cp932"
  content <- readFileEncoding cp932 "./shift-jis.txt"
  putStrLn content
