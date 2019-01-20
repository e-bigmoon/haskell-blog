#!/usr/bin/env stack
-- stack script --resolver lts-13.4

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.Default         (def)
import           Data.Text            (Text)
import           Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy -- just for testing, don't use in real life!!!

data App = App
    { httpManager :: Manager
    }

mkYesod "App" [parseRoutes|
/      HomeR  GET POST
/admin AdminR GET
/auth  AuthR  Auth getAuth
|]

instance Yesod App where
    authRoute _ = Just $ AuthR LoginR

    -- route name, then a boolean indicating if it's a write request
    isAuthorized HomeR True = isAdmin
    isAuthorized AdminR _ = isAdmin

    -- anyone can access other pages
    isAuthorized _ _ = return Authorized

isAdmin = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "You must be an admin"

instance YesodAuth App where
    type AuthId App = Text
    authenticate = return . Authenticated . credsIdent

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins _ = [authDummy]

    maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Note: Log in as "admin" to be an administrator.
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            <p>
                <a href=@{AdminR}>Go to admin page
            <form method=post>
                Make a change (admins only)
                \ #
                <input type=submit>
        |]

postHomeR :: Handler ()
postHomeR = do
    setMessage "You made some change to the page"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = defaultLayout
    [whamlet|
        <p>I guess you're an admin!
        <p>
            <a href=@{HomeR}>Return to homepage
    |]

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    warp 3000 $ App manager