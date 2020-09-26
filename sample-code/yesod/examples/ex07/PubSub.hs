#!/usr/bin/env stack
-- stack script --resolver lts-16.12
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap
import           Data.Text              (Text)
import           Yesod

data App = App
    { jobs    :: TVar (IntMap (TChan (Maybe Text)))
    , nextJob :: TVar Int
    }

mkYesod "App" [parseRoutes|
/ HomeR GET POST
/view-progress/#Int ViewProgressR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "PubSub example"
    [whamlet|
        <form method=post>
            <button>Start new background job
    |]

postHomeR :: Handler ()
postHomeR = do
    App {..} <- getYesod
    (jobId, chan) <- liftIO $ atomically $ do
        jobId <- readTVar nextJob
        writeTVar nextJob $! jobId + 1
        chan <- newBroadcastTChan
        m <- readTVar jobs
        writeTVar jobs $ IntMap.insert jobId chan m
        return (jobId, chan)
    liftIO $ forkIO $ do
        threadDelay 1000000
        atomically $ writeTChan chan $ Just "Did something\n"
        threadDelay 1000000
        atomically $ writeTChan chan $ Just "Did something else\n"
        threadDelay 1000000
        atomically $ do
            writeTChan chan $ Just "All done\n"
            writeTChan chan Nothing
            m <- readTVar jobs
            writeTVar jobs $ IntMap.delete jobId m
    redirect $ ViewProgressR jobId

getViewProgressR :: Int -> Handler TypedContent
getViewProgressR jobId = do
    App {..} <- getYesod
    mchan <- liftIO $ atomically $ do
        m <- readTVar jobs
        case IntMap.lookup jobId m of
            Nothing -> return Nothing
            Just chan -> fmap Just $ dupTChan chan
    case mchan of
        Nothing -> notFound
        Just chan -> respondSource typePlain $ do
            let loop = do
                    mtext <- liftIO $ atomically $ readTChan chan
                    case mtext of
                        Nothing -> return ()
                        Just text -> do
                            sendChunkText text
                            sendFlush
                            loop
            loop

main :: IO ()
main = do
    jobs <- newTVarIO IntMap.empty
    nextJob <- newTVarIO 1
    warp 3001 App {..}