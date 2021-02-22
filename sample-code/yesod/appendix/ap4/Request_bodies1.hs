#!/usr/bin/env stack
-- stack script --resolver lts-17.3
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

data Person = Person String Int
instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age"  .= age
        ]

people :: [Person]
people = [Person "Alice" 30, Person "Bob" 35, Person "Charlie" 40]

main :: IO ()
main = do
    let request = setRequestBodyJSON people $ "POST https://httpbin.org/post"
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)