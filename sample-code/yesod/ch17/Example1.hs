#!/usr/bin/env stack
-- stack script --resolver lts-13.26
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Types         (status200)
import           Network.Wai                (pathInfo, rawPathInfo,
                                             requestMethod, responseLBS)
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/only-get       OnlyGetR   GET
/any-method     AnyMethodR
/has-param/#Int HasParamR  GET
/my-subsite     MySubsiteR WaiSubsite getMySubsite
|]

instance Yesod App

getOnlyGetR :: Handler Html
getOnlyGetR = defaultLayout
    [whamlet|
        <p>Accessed via GET method
        <form method=post action=@{AnyMethodR}>
            <button>POST to /any-method
    |]

handleAnyMethodR :: Handler Html
handleAnyMethodR = do
    req <- waiRequest
    defaultLayout
        [whamlet|
            <p>In any-method, method == #{show $ requestMethod req}
        |]

getHasParamR :: Int -> Handler String
getHasParamR i = return $ show i

getMySubsite :: App -> WaiSubsite
getMySubsite _ =
    WaiSubsite app
  where
    app req sendResponse = sendResponse $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        $ L8.pack $ concat
            [ "pathInfo == "
            , show $ pathInfo req
            , ", rawPathInfo == "
            , show $ rawPathInfo req
            ]

main :: IO ()
main = warp 3000 App