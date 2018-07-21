module Quiz3 where

import           Data.Char       (isDigit)
import           Test.QuickCheck

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
