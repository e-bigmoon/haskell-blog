#!/usr/bin/env stack
-- stack script --resolver lts-14.19
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
import           Blaze.ByteString.Builder           (fromByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import           Control.Concurrent                 (threadDelay)
import           Control.Monad                      (forM_)
import           Data.Monoid                        ((<>))
import           Network.Wai                        (pathInfo)
import           Yesod.Core                         (HandlerT, RenderRoute (..),
                                                     TypedContent, Yesod,
                                                     YesodDispatch (..), liftIO,
                                                     notFound, respondSource,
                                                     sendChunk, sendChunkBS,
                                                     sendChunkText, sendFlush,
                                                     warp, yesodRunner)

-- | Our foundation datatype.
data App = App

instance Yesod App

instance RenderRoute App where
    data Route App = HomeR -- just one accepted URL
        deriving (Show, Read, Eq, Ord)

    renderRoute HomeR = ( [] -- empty path info, means "/"
                        , [] -- empty query string
                        )

getHomeR :: HandlerT App IO TypedContent
getHomeR = respondSource "text/plain" $ do
    sendChunkBS "Starting streaming response.\n"
    sendChunkText "Performing some I/O.\n"
    sendFlush
    -- pretend we're performing some I/O
    liftIO $ threadDelay 1000000
    sendChunkBS "I/O performed, here are some results.\n"
    forM_ [1..50 :: Int] $ \i -> do
        sendChunk $ fromByteString "Got the value: " <>
                    fromShow i <>
                    fromByteString "\n"

instance YesodDispatch App where
    yesodDispatch yesodRunnerEnv req sendResponse =
        let maybeRoute =
                case pathInfo req of
                    [] -> Just HomeR
                    _  -> Nothing
            handler =
                case maybeRoute of
                    Nothing -> notFound
                    Just HomeR -> getHomeR
         in yesodRunner handler yesodRunnerEnv maybeRoute req sendResponse

main :: IO ()
main = warp 3000 App
