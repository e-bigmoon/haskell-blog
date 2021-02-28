#!/usr/bin/env stack
-- stack script --resolver lts-17.3
{-# LANGUAGE OverloadedStrings #-}
import           Network.HTTP.Client 
import           Network.HTTP.Client.TLS
import           Text.HTML.DOM
import           Data.Conduit ((.|), runConduit, runConduitRes, ConduitT)
import           Conduit (runResourceT)
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.Connection         (TLSSettings(TLSSettingsSimple))
import qualified System.IO as SI (readFile)
import           Text.HTML.DOM
import           Network.HTTP.Types
import           Text.XML
import           Data.Text (Text)
import qualified Data.Map.Strict  as DMS
import           Data.Maybe
import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T
import Text.HTML.DOM (readFile)

-- main :: IO ()
-- main = do
--     --doc <- readFile def "test4.xml"
--     doc <- Text.HTML.DOM.readFile "test4.html"
--     let cursor = fromDocument doc
--     print $ T.concat $
--         cursor $// element "body"
--                -- >=> attributeIs "class" "bar"
--                &// attribute "href"

main :: IO ()
main = do
    filecontent <- SI.readFile "urls.txt"
    let urls = map parseRequest_ $ lines filecontent
        settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    _ <- mapM (doSomething manager) urls
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