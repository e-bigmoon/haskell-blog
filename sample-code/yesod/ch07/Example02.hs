#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package text
    --package yesod
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import           Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/wiki/*Page WikiR GET
|]

instance Yesod App

data Page = Page Text Text [Text] -- 2 or more
  deriving (Eq, Show, Read)

instance PathMultiPiece Page where
    toPathMultiPiece (Page x y z) = x : y : z
    fromPathMultiPiece (x:y:z) = Just $ Page x y z
    fromPathMultiPiece _       = Nothing

getWikiR :: Page -> Handler Html
getWikiR (Page lv1 lv2 lvs) = defaultLayout
  [whamlet|
    #{lv1} > #{lv2}
    $forall underLayers <- lvs
      \ > #{underLayers} #

  |]

main :: IO ()
main = warp 3000 App
