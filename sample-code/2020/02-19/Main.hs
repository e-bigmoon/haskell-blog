import Test.HUnit hiding ((@?=))
import qualified Test.HUnit as HUnit ((@?=))
import Text.Show.Unicode

bad :: IO ()
bad = runTestTT (TestCase $ "Haskell" HUnit.@?= "ハスケル") >> return ()

newtype UString a = UString a
  deriving (Eq)

instance Show a => Show (UString a) where
  show (UString s) = ushow s

(@?=) :: (Eq a, Show a) => a -> a -> Assertion
actual @?= expected = UString actual HUnit.@?= UString expected

good :: IO ()
good = runTestTT (TestCase $ "Haskell" @?= "ハスケル") >> return ()