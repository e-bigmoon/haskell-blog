module Quiz3 where

import Test.QuickCheck
import Data.Char (isDigit)

newtype Digit = Digit Char
  deriving Show

propIsDigit :: Digit -> Bool
propIsDigit (Digit c) = isDigit c

-- こたえ1
instance Arbitrary Digit where
  arbitrary = Digit <$> elements "1234567890"

-- こたえ2
-- instance Arbitrary Digit where
--   arbitrary = Digit <$> arbitrary `suchThat` isDigit