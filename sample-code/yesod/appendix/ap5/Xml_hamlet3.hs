#!/usr/bin/env stack
-- stack script --resolver lts-16.29
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Text.XML
import Text.Hamlet.XML
import Prelude hiding (writeFile)
import Data.Text (Text, pack)
import Data.Map (empty)

data Person = Person
    { personName :: Text
    , personAge :: Int
    }

people :: [Person]
people =
    [ Person "Michael" 26
    , Person "Miriam" 25
    , Person "Eliezer" 3
    , Person "Gavriella" 1
    ]

main :: IO ()
main =
    writeFile def "people.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "html" empty [xml|
<head>
    <title>Some People
<body>
    <h1>Some People
    $if null people
        <p>There are no people.
    $else
        <dl>
            $forall person <- people
                ^{personNodes person}
|]

personNodes :: Person -> [Node]
personNodes person = [xml|
<dt>#{personName person}
<dd>#{pack $ show $ personAge person}
|]