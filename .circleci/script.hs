#!/usr/bin/env stack
-- stack --resolver lts-10.5 script
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Default.Class
import           Data.List
import qualified Data.Text             as T
import           Network.HTTP.Req
import           System.Environment
import           Turtle                hiding (header)

main :: IO ()
main = do
  pullRequestUrl <- lookupEnv "CIRCLE_PULL_REQUEST"
  maybe (putStrLn "This build is not PR.") main' pullRequestUrl

main' :: String -> IO ()
main' pullRequestUrl = do
  putStrLn ("PR: " ++ pullRequestUrl)
  view $ shell "stack build" empty
  view $ shell "stack exec site rebuild" empty
  runReq def $ do
    userName <- liftIO $ getEnv "CIRCLE_PROJECT_USERNAME"
    repoName <- liftIO $ getEnv "CIRCLE_PROJECT_REPONAME"
    buildUrl <- liftIO $ getEnv "CIRCLE_BUILD_URL"
    homeDir  <- liftIO $ getEnv "HOME"
    let prNumber = reverse . takeWhile (/= '/') $ reverse pullRequestUrl
    token <- liftIO $ getEnv "GITHUB_OAUTH"
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
    r <- req
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
      (oAuth2Token (B.pack token) <> header "User-Agent" "bigmoon")
    liftIO $ print (responseBody r :: Value)

