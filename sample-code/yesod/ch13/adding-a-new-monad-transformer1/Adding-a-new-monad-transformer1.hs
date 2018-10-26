{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
import Yesod
import Crypto.Random (SystemRandom, newGenIO)
import Control.Monad.CryptoRandom
import Data.ByteString.Base16 (encode)
import Data.Text.Encoding (decodeUtf8)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    gen <- liftIO newGenIO
    eres <- evalCRandT (getBytes 16) (gen :: SystemRandom)
    randomBS <-
        case eres of
            Left e -> error $ show (e :: GenError)
            Right gen -> return gen
    defaultLayout
        [whamlet|
            <p>Here's some random data: #{decodeUtf8 $ encode randomBS}
        |]

main :: IO ()
main = warp 3000 App