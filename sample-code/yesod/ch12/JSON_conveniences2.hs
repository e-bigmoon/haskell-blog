#!/usr/bin/env stack
-- stack script --resolver lts-12.9

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text (Text)
import           Yesod

data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Value
getHomeR = returnJson $ Person "Michael" 28

main :: IO ()
main = warp 3000 App
