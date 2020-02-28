#!/usr/bin/env stack
-- stack script --resolver lts-14.27
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
import           Network.HTTP.Types            (status200)
import           Network.Wai                   (responseBuilder)
import           Network.Wai.Handler.Warp      (run)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import qualified Text.Blaze.Html5              as H
import           Yesod.Core                    (Html, RenderRoute (..), Yesod,
                                                YesodDispatch (..), toWaiApp)
import           Yesod.Core.Types              (YesodRunnerEnv (..))

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

instance YesodDispatch App where
    yesodDispatch (YesodRunnerEnv _logger site _sessionBackend _ _) _req sendResponse =
        sendResponse $ responseBuilder
            status200
            [("Content-Type", "text/html")]
            (renderHtmlBuilder $ welcomeMessage site)

main :: IO ()
main = do
    -- We could get this message from a file instead if we wanted.
    let welcome = H.p "Welcome to Yesod!"
    waiApp <- toWaiApp App
        { welcomeMessage = welcome
        }
    run 3000 waiApp