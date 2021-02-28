#!/usr/bin/env stack
-- stack script --resolver lts-16.29
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Data.Map        (empty)
import           Prelude         hiding (writeFile)
import           Text.Hamlet.XML
import           Text.XML

main :: IO ()
main =
    writeFile def "test3.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "html" empty [xml|
<head>
    <title>
        My #
        <b>Title
<body>
    <p>foo bar baz
|]