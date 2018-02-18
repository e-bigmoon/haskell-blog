#!/usr/bin/env stack
-- stack --resolver lts-10.5 script
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}

module Main where

import           Control.Exception     (throwIO)
import           Data.Aeson
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import           Data.Default.Class    (def)
import           Data.Function         ((&))
import           Data.List             (intercalate)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T (pack)
import           Network.HTTP.Req
import           System.Environment    (lookupEnv)
import           Turtle                hiding (header)


main :: IO ()
main = do
  -- | If this build is part of only one pull request, its URL will be populated here. If there was more than one pull request, it will contain one of the pull request URLs (picked randomly).
  pullRequestUrl <- lookupEnv "CIRCLE_PULL_REQUEST"
  flip (maybe $ putStrLn "This build is not PR.") pullRequestUrl $ \prUrl -> do
    putStrLn ("PR: " ++ prUrl)

    -- | See https://circleci.com/docs/2.0/env-vars/#build-details
    -- | The username or organization name of the project being tested, i.e. “foo” in circleci.com/gh/foo/bar/123.
    userName <- lookupEnvWithError "CIRCLE_PROJECT_USERNAME"
    -- | The repository name of the project being tested, i.e. “bar” in circleci.com/gh/foo/bar/123.
    repoName <- lookupEnvWithError "CIRCLE_PROJECT_REPONAME"
    -- | The URL for the current build.
    buildUrl <- lookupEnvWithError "CIRCLE_BUILD_URL"

    -- | custom environments
    homeDir  <- lookupEnvWithError "HOME"
    token    <- lookupEnvWithError "GITHUB_OAUTH"

    view $ do
      shell "stack build"             empty
      shell "stack exec site rebuild" empty

    let
      prNumber = takeWhileEnd (/= '/') prUrl
      url = mkUrl (T.pack userName) (T.pack repoName) (T.pack prNumber)
      message = mkMessage buildUrl homeDir repoName

    resp <- runReq def $ mkReq url message (B.pack token)
    print $ responseBody resp

mkReq :: Url Https -> String -> ByteString -> Req (JsonResponse Value)
mkReq url message token =
  req POST url (ReqBodyJson $ object [ "body" .= message]) jsonResponse $
    oAuth2Token token <> header "User-Agent" "bigmoon"

mkMessage :: String -> String -> String -> String
mkMessage buildUrl homeDir repoName =
  intercalate "/" [buildUrl, "artifacts", "0", homeDir, repoName, "_site/index.html"]

mkUrl :: Text -> Text -> Text -> Url Https
mkUrl userName repoName prNumber =
  https "api.github.com" /: "repos" /: userName /: repoName /: "issues" /: prNumber /: "comments"

-- | port from dropWhileEnd in Data.List
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = snd . foldr go (False, [])
 where
  go x (isFinish, xs) | not isFinish && p x = (False, x : xs)
                      | otherwise           = (True, xs)

lookupEnvWithError :: String -> IO String
lookupEnvWithError env =
  maybe (throwIO $ env ++ " is not found") pure =<< lookupEnv env
