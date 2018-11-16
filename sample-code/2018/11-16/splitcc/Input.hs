{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Input (ts, ansSplit, ansFold) where

import           Data.Char          (isSpace, isUpper)
import           Data.List.Split    (split, startsWithOneOf)

import           GHC.Generics    (Generic)
import           Control.DeepSeq

import           Data.Default         (def)
import           AutoBench.Types      (DataOpts(..), TestSuite(..))
import           AutoBench.QuickCheck ()
import           Test.QuickCheck

ansSplit :: MyString -> String
ansSplit = unwords . split (startsWithOneOf ['A'..'Z']) . getString

ansFold :: MyString -> String
ansFold = fmt . foldr go [] . getString
  where
    go c acc
      | isUpper c = ' ':c:acc
      | otherwise = c:acc
    fmt cs
      | null cs = cs
      | isSpace (head cs) = tail cs
      | otherwise = cs

ts :: TestSuite
ts  = def { _dataOpts = Gen 0 10000 200000 }

newtype MyString = MyString { getString :: String }
  deriving (Eq, Show, Generic, NFData)

instance Arbitrary MyString where
  arbitrary = fmap MyString $ listOf $ elements (['a'..'z']++['A'..'Z'])