#!/usr/bin/env stack
-- stack script --resolver lts-17.3
{-# LANGUAGE OverloadedStrings #-}
import           Prelude hiding (readFile)
import           Network.HTTP.Client 
import           Network.HTTP.Client.TLS
import           Text.HTML.DOM
import           Data.Conduit ((.|), runConduit, runConduitRes, ConduitT)
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.Connection         (TLSSettings(TLSSettingsSimple))
import qualified System.IO as SI (readFile)
import           Network.HTTP.Types
import           Text.XML
import           Data.Text (Text)
import           Text.XML.Cursor
import qualified Data.Text as T

main :: IO ()
main = do
    let url = "https://hackage.haskell.org/package/xml-types-0.3.6/docs/Data-XML-Types.html#t:Content"
        settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    _ <- doSomething manager url
    pure ()

doSomething :: Manager -> Request -> IO ()
doSomething manager request = do
    withResponse request manager $ \response -> do
        valueDoc <- runConduit $ bodyReaderSource (responseBody response)
              .| Text.HTML.DOM.sinkDoc
        print $ takeUrl valueDoc

takeUrl :: Document -> Text
takeUrl doc = T.concat $ 
            fromDocument doc $// element "body"
                             &// attribute "href"