module Main (main) where

import Test.QuickCheck

import SplitCC

newtype MyString = MyString { getString :: String }
  deriving (Eq, Show)

instance Arbitrary MyString where
  arbitrary = fmap MyString $ listOf $ elements (['a'..'z']++['A'..'Z'])


main :: IO ()
main = quickCheck prop_split

prop_split :: MyString -> Bool
prop_split xs = ansSplit xs' == ansFold xs'
  where xs' = getString xs