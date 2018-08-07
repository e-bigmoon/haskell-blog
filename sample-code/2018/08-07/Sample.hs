import           Control.Arrow
import           Data.Char     (toUpper)
import           Data.Text     (Text)
import qualified Data.Text     as T

toAllUpper :: String -> String
toAllUpper = map toUpper

toAllUpperText :: Text -> Text
toAllUpperText = T.pack . toAllUpper . T.unpack

toAllUpperText' :: Text -> Text
toAllUpperText' = T.unpack ^>> toAllUpper >>^ T.pack

toAllUpperText'' :: Text -> Text
toAllUpperText'' = T.pack <<^ toAllUpper ^<< T.unpack
