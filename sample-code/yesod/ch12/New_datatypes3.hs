#!/usr/bin/env stack
-- stack script --resolver lts-12.9

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
import           Data.Text (Text)
import           Yesod

data Person = Person
    { name :: Text
    , age  :: Int
    }
    deriving Show

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

mimeType :: ContentType
mimeType = "text/haskell-show"

data HaskellShow = forall a. Show a => HaskellShow a

instance ToContent HaskellShow where
    toContent (HaskellShow x) = toContent $ show x
instance ToTypedContent HaskellShow where
    toTypedContent = TypedContent mimeType . toContent
instance HasContentType HaskellShow where
    getContentType _ = mimeType

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    provideRep $ return $ HaskellShow person
    provideJson person
  where
    person = Person "Michael" 28

main :: IO ()
main = warp 3000 App
