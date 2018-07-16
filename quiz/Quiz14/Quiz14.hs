#!/usr/bin/env stack
-- stack script --resolver lts-12.0

import System.IO.Extra

main :: IO ()
main = do
  cp932 <- mkTextEncoding "cp932"
  content <- readFileEncoding cp932 "./shift-jis.txt"
  putStrLn content

bad :: IO ()
bad = readFile "./shift-jis.txt" >>= putStrLn

good :: IO ()
good = readFile "./utf8.txt" >>= putStrLn