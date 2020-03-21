#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package aeson
    --package bytestring
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Text                  (Text)

data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]

main :: IO ()
main = L.putStrLn $ encode $ Person "Michael" 28
