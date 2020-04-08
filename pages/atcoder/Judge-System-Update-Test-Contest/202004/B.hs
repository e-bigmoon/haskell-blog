{-# LANGUAGE PartialTypeSignatures #-}
module Main (main) where

import Control.Monad
import qualified Data.ByteString.Char8 as C8
import qualified Data.Char as C
import qualified Data.List as L
import Data.Maybe

main :: IO ()
main = do
  [n] <- readInts
  replicateM n readStrings >>= output . solve . map read'

data Color = R | B
  deriving (Eq, Ord, Read)

read' :: [String] -> (Color, Int)
read' [i, c] = (read c, read i)

solve :: [(Color, Int)] -> [Int]
solve = map snd . L.sort

output :: _ -> IO ()
output = mapM_ print

-- utils
readInts :: IO [Int]
readInts = L.unfoldr (C8.readInt . C8.dropWhile C.isSpace) <$> C8.getLine

readStrings :: IO [String]
readStrings = map C8.unpack . C8.words <$> C8.getLine