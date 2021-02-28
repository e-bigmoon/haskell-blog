#!/usr/bin/env stack
-- stack script --resolver lts-16.29
{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T

main :: IO ()
main = do
    doc <- readFile def "test2.xml"
    let cursor = fromDocument doc
    print $ T.concat $
        cursor $// element "h2"
               >=> attributeIs "class" "bar"
               >=> precedingSibling
               >=> element "h1"
               &// content