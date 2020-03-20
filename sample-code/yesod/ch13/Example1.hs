#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package base16-bytestring
    --package bytestring
    --package crypto-api
    --package monadcryptorandom
    --package text
    --package yesod
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Control.Monad.CryptoRandom
import           Crypto.Random              (SystemRandom, newGenIO)
import           Data.ByteString.Base16     (encode)
import           Data.Text.Encoding         (decodeUtf8)
import           Yesod

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
            Left e    -> error $ show (e :: GenError)
            Right gen -> return gen
    defaultLayout
        [whamlet|
            <p>Here's some random data: #{decodeUtf8 $ encode randomBS}
        |]

main :: IO ()
main = warp 3000 App
