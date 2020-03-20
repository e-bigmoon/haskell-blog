#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package text
    --package yesod
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
import           Data.Text (Text)
import           Yesod

data Person = Person
    { name :: Text
    , age  :: Int
    }
    deriving Show

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

mimeType :: ContentType
mimeType = "text/haskell-show"

data HaskellShow = forall a. Show a => HaskellShow a

instance ToContent HaskellShow where
    toContent (HaskellShow x) = toContent $ show x
instance ToTypedContent HaskellShow where
    toTypedContent = TypedContent mimeType . toContent

getHomeR :: Handler HaskellShow
getHomeR =
    return $ HaskellShow person
  where
    person = Person "Michael" 28

main :: IO ()
main = warp 3000 App
