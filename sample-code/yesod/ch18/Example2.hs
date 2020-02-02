#!/usr/bin/env stack
-- stack script --resolver lts-14.22
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
import           Yesod.Core.Types           (YesodSubRunnerEnv (..))

data App = App

instance RenderRoute App where
    data Route App = OnlyGetR
                   | AnyMethodR
                   | HasParamR Int
                   | MySubsiteR (Route WaiSubsite)
        deriving (Show, Read, Eq)

    renderRoute OnlyGetR = (["only-get"], [])
    renderRoute AnyMethodR = (["any-method"], [])
    renderRoute (HasParamR i) = (["has-param", toPathPiece i], [])
    renderRoute (MySubsiteR subRoute) =
        let (ps, qs) = renderRoute subRoute
         in ("my-subsite" : ps, qs)

type Handler = HandlerT App IO

instance Yesod App

instance YesodDispatch App where
    yesodDispatch env req =
        case pathInfo req of
            ["only-get"] ->
                case requestMethod req of
                    "GET" -> yesodRunner
                        getOnlyGetR
                        env
                        (Just OnlyGetR)
                        req
                    _ -> yesodRunner
                        (badMethod >> return ())
                        env
                        (Just OnlyGetR)
                        req
            ["any-method"] ->
                yesodRunner handleAnyMethodR env (Just AnyMethodR) req
            ["has-param", t] | Just i <- fromPathPiece t ->
                case requestMethod req of
                    "GET" -> yesodRunner
                        (getHasParamR i)
                        env
                        (Just $ HasParamR i)
                        req
                    _ -> yesodRunner
                        (badMethod >> return ())
                        env
                        (Just $ HasParamR i)
                        req
            ("my-subsite":rest) -> yesodSubDispatch
                YesodSubRunnerEnv
                    { ysreGetSub = getMySubsite
                    , ysreParentRunner = yesodRunner
                    , ysreToParentRoute = MySubsiteR
                    , ysreParentEnv = env
                    }
                req { pathInfo = rest }
            _ -> yesodRunner (notFound >> return ()) env Nothing req

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