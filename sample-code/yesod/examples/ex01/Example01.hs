#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package markdown
    --package text
    --package yesod
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.IORef
import qualified Data.Text.Lazy.IO as TLIO
import           Text.Markdown
import           Yesod

data App = App
    { homepageContent :: Html
    , visitorCount    :: IORef Int
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]
instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    App {..} <- getYesod
    currentCount <- liftIO $ atomicModifyIORef visitorCount
        $ \i -> (i + 1, i + 1)
    defaultLayout $ do
        setTitle "Homepage"
        [whamlet|
            <article>#{homepageContent}
            <p>You are visitor number: #{currentCount}.
        |]

main :: IO ()
main = do
    rawMarkdown <- TLIO.readFile "homepage.md"
    countRef <- newIORef 0
    warp 3000 App
        { homepageContent = markdown def rawMarkdown
        , visitorCount    = countRef
        }
