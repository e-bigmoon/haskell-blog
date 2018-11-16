module SplitCC
  ( splitCC
  , foldSplitCC
  ) where

import           Data.Char       (isSpace, isUpper)
import           Data.List.Split (split, startsWithOneOf)

splitCC :: String -> String
splitCC  = unwords . split (startsWithOneOf ['A'..'Z'])

foldSplitCC :: String -> String
foldSplitCC = fmt . foldr go []
  where
    go c acc
      | isUpper c = ' ':c:acc
      | otherwise = c:acc
    fmt cs
      | null cs = cs
      | isSpace (head cs) = tail cs
      | otherwise = cs
