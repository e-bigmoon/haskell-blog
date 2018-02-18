#!/usr/bin/env stack
-- stack --resolver lts-10.5 script
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Char8      as B (pack)
import           Data.Default.Class         (def)
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T (pack)
import           Network.HTTP.Req
import           System.Environment         (lookupEnv)
import           Turtle                     hiding (header)

main :: IO ()
main = do
  -- | If this build is part of only one pull request, its URL will be populated here. If there was more than one pull request, it will contain one of the pull request URLs (picked randomly).
  pullRequestUrl <- lookupEnv "CIRCLE_PULL_REQUEST"
  maybe (putStrLn "This build is not PR.") main' pullRequestUrl

main' :: String -> IO ()
main' prUrl = do
  -- | See https://circleci.com/docs/2.0/env-vars/#build-details
  -- | The username or organization name of the project being tested, i.e. “foo” in circleci.com/gh/foo/bar/123.
  userName <- fromMaybe "" <$> lookupEnv "CIRCLE_PROJECT_USERNAME"
  -- | The repository name of the project being tested, i.e. “bar” in circleci.com/gh/foo/bar/123.
  repoName <- fromMaybe "" <$> lookupEnv "CIRCLE_PROJECT_REPONAME"
  -- | The URL for the current build.
  buildUrl <- fromMaybe "" <$> lookupEnv "CIRCLE_BUILD_URL"

  -- | custom environments
  homeDir  <- fromMaybe "" <$> lookupEnv "HOME"
  token    <- fromMaybe "" <$> lookupEnv "GITHUB_OAUTH"

  putStrLn ("PR: " ++ prUrl)

  view $ do
    shell "stack build"             empty
    shell "stack exec site rebuild" empty

  runReq def $ do
    let prNumber = takeWhileEnd (/= '/') prUrl
    let payload = object
          [ "body" .= intercalate
              "/"
              [ buildUrl
              , "artifacts"
              , "0"
              , homeDir
              , repoName
              , "_site/index.html"
              ]
          ]
    request <- req
      POST
      (  https "api.github.com"
      /: "repos"
      /: T.pack userName
      /: T.pack repoName
      /: "issues"
      /: T.pack prNumber
      /: "comments"
      )
      (ReqBodyJson payload)
      jsonResponse
      (oAuth2Token (B.pack token) <> header "User-Agent" "CircleCI Preview")
    liftIO $ print (responseBody request :: Value)

-- | port from dropWhileEnd in Data.List
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = snd . foldr go (False, [])
 where
  go x (isFinish, xs) | not isFinish && p x = (False, x : xs)
                      | otherwise           = (True, xs)


