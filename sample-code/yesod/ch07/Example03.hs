#!/usr/bin/env stack
-- stack script --resolver lts-13.9
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import           Yesod

import Data.Text (Text)

data App = App

mkYesod "App" [parseRoutes|
/foo/bar    Foo1R GET
!/foo/#Int  Foo2R GET
!/foo/#Text Foo3R GET
|]

instance Yesod App

getFoo1R :: Handler Html
getFoo1R = defaultLayout [whamlet|Foo1|]

getFoo2R :: Int -> Handler Html
getFoo2R i = defaultLayout [whamlet|Foo2. parameter is #{i}|]

getFoo3R :: Text -> Handler Html
getFoo3R s = defaultLayout [whamlet|Foo3. parameter is #{s}|]

main :: IO ()
main = warp 3000 App
