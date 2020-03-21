#!/usr/bin/env stack
-- stack script --resolver lts-14.27
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
import           Network.Wai              (pathInfo)
import           Network.Wai.Handler.Warp (run)
import qualified Text.Blaze.Html5         as H
import           Yesod.Core               (HandlerT, Html, RenderRoute (..),
                                           Yesod, YesodDispatch (..), getYesod,
                                           notFound, toWaiApp, yesodRunner)

-- | Our foundation datatype.
data App = App
    { welcomeMessage :: !Html
    }

instance Yesod App

instance RenderRoute App where
    data Route App = HomeR -- just one accepted URL
        deriving (Show, Read, Eq, Ord)

    renderRoute HomeR = ( [] -- empty path info, means "/"
                        , [] -- empty query string
                        )

getHomeR :: HandlerT App IO Html
getHomeR = do
    site <- getYesod
    return $ welcomeMessage site

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
main = do
    -- We could get this message from a file instead if we wanted.
    let welcome = H.p "Welcome to Yesod!"
    waiApp <- toWaiApp App
        { welcomeMessage = welcome
        }
    run 3000 waiApp