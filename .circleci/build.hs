#!/usr/bin/env stack
-- stack --resolver lts-10.6 script
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment (lookupEnv)
import           Turtle

main :: IO ()
main = do
  -- | If this build is part of only one pull request, its URL will be populated here. If there was more than one pull request, it will contain one of the pull request URLs (picked randomly).
  pullRequestUrl <- lookupEnv "CIRCLE_PULL_REQUEST"
  flip (maybe $ putStrLn "This build is not PR.") pullRequestUrl $ \prUrl -> do
    putStrLn ("PR: " ++ prUrl)

    eCode <- shell "stack build" empty .&&. shell "stack exec site rebuild" empty
    exit eCode