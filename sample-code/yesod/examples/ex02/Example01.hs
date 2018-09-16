#!/usr/bin/env stack
-- stack script --resolver lts-12.9
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.IORef
import           Yesod

newtype App = App
    { visitors :: IORef Int
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    visitorsRef <- fmap visitors getYesod
    visitors <-
        liftIO $ atomicModifyIORef visitorsRef $ \i ->
        (i + 1, i + 1)
    defaultLayout
        [whamlet|
            <p>Welcome, you are visitor number #{visitors}.
        |]

main :: IO ()
main = do
    visitorsRef <- newIORef 0
    warp 3000 App
        { visitors = visitorsRef
        }
