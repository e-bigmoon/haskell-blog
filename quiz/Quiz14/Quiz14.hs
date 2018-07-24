#!/usr/bin/env stack
-- stack script --resolver lts-12.0

main :: IO ()
main = do
  good
  bad

bad :: IO ()
bad = readFile "./shift-jis.txt" >>= putStrLn

good :: IO ()
good = readFile "./utf8.txt" >>= putStrLn
