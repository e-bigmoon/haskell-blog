#!/usr/bin/env stack
-- stack script --resolver lts-12.9
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.Set         (member)
import           Data.Text        (Text)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/unprotected UnprotectedR GET
/admin AdminR:
    /1 Admin1R GET
    /2 Admin2R GET
    /3 Admin3R GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    authRoute _ = Just $ AuthR LoginR
    isAuthorized (AdminR _) _writable = do
        muser <- maybeAuthId
        case muser of
            Nothing -> return AuthenticationRequired
            Just ident
                -- Just a hack since we're using the dummy module
                | ident == "admin" -> return Authorized
                | otherwise -> return $ Unauthorized "Admin access only"
    isAuthorized _route _writable = return Authorized

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Hacky YesodAuth instance for just the dummy auth plugin
instance YesodAuth App where
    type AuthId App = Text

    loginDest _ = HomeR
    logoutDest _ = HomeR
    getAuthId = return . Just . credsIdent
    authPlugins _ = [authDummy]
    maybeAuthId = lookupSession credsKey

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Route attr homepage"
    [whamlet|
        <p>
            <a href=@{UnprotectedR}>Unprotected
        <p>
            <a href=@{AdminR Admin1R}>Admin 1
        <p>
            <a href=@{AdminR Admin2R}>Admin 2
        <p>
            <a href=@{AdminR Admin3R}>Admin 3
    |]

getUnprotectedR, getAdmin1R, getAdmin2R, getAdmin3R :: Handler Html
getUnprotectedR = defaultLayout [whamlet|Unprotected|]
getAdmin1R = defaultLayout [whamlet|Admin1|]
getAdmin2R = defaultLayout [whamlet|Admin2|]
getAdmin3R = defaultLayout [whamlet|Admin3|]

main :: IO ()
main = warp 3000 App