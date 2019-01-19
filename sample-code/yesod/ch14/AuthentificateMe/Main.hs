{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.Default                (def)
import           Data.Text as T 
import           LoadEnv
import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.Wai.Handler.Warp (runEnv)
import           System.Environment (getEnv)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.GoogleEmail2
import           Yesod.Auth.OAuth2.Google

-- Replace with Google client ID.
clientId :: Text
clientId = "Google client ID"

-- Replace with Google secret ID.
clientSecret :: Text
clientSecret = "Google secret ID"

data App = App
    { appHttpManager :: Manager
    , appAuthPlugins :: [AuthPlugin App]
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    -- Note: In order to log in with BrowserID, you must correctly
    -- set your hostname here.
    approot = ApprootStatic "http://localhost:3000"

instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins = appAuthPlugins
    maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

mkFoundation :: IO App
mkFoundation = do
    loadEnv
    appHttpManager <- newManager
    appAuthPlugins <- sequence [loadPlugin oauth2Google "GOOGLE"]
    return App{..}
  where
    loadPlugin f prefix = do
        clientId <- getEnv $ prefix <> "_CLIENT_ID"
        clientSecret <- getEnv $ prefix <> "_CLIENT_SECRET"
        pure $ f (T.pack clientId) (T.pack clientSecret)

main :: IO ()
main = runEnv 3000 =<< toWaiApp =<< mkFoundation
