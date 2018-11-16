module SplitCC
  ( ansSplit
  , ansFold
  ) where

import           Data.Char       (isSpace, isUpper)
import           Data.List.Split (split, startsWithOneOf)

ansSplit :: String -> String
ansSplit = unwords . split (startsWithOneOf ['A'..'Z'])

ansFold :: String -> String
ansFold = fmt . foldr go []
  where
    go c acc
      | isUpper c = ' ':c:acc
      | otherwise = c:acc
    fmt cs
      | null cs = cs
      | isSpace (head cs) = tail cs
      | otherwise = cs
