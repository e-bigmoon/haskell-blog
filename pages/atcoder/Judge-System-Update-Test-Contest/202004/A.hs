{-# LANGUAGE PartialTypeSignatures #-}
module Main (main) where

import qualified Data.ByteString.Char8 as C8
import Data.Maybe

main :: IO ()
main = readInts >>= output . solve

solve :: [Int] -> _
solve [s,l,r] = min (max l s) r

output :: _ -> IO ()
output = print

-- utils
readInts :: IO [Int]
readInts = map (fst . fromJust . C8.readInt) . C8.words <$> C8.getLine