{-# LANGUAGE PartialTypeSignatures #-}
module Main (main) where

import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import Data.Maybe

main :: IO ()
main = readInts >>= output . solve

solve :: [Int] -> Int
solve xs
  = length
  $ filter ((\[l1,l2,l3] -> checkRow l1 l2 l3 && checkCol l1 l2 l3) . split xs)
  $ L.permutations [1..n] 
  where
    n = sum xs

split :: [Int] -> [Int] -> [[Int]]
split [k1,k2,k3] xs = [l1,l2,l3]
  where
    (l1, rest1) = splitAt k1 xs
    (l2, rest2) = splitAt k2 rest1
    (l3, _rest) = splitAt k3 rest2

checkRow :: [Int] -> [Int] -> [Int] -> Bool
checkRow l1 l2 l3 = isAscList l1 && isAscList l2 && isAscList l3
  where
    isAscList = and . (zipWith (<) <*> tail)

checkCol :: [Int] -> [Int] -> [Int] -> Bool
checkCol l1@(~(x:xs)) l2@(~(y:ys)) l3@(~(z:zs))
  | null l3 && null l2 = True
  | null l3 = x < y && checkCol xs ys l3
  | otherwise = x < y && y < z && checkCol xs ys zs

output :: _ -> IO ()
output = print

-- utils
readInts :: IO [Int]
readInts = map (fst . fromJust . C8.readInt) . C8.words <$> C8.getLine
