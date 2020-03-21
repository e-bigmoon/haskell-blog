#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package yesod-core
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import           Yesod.Core (RenderRoute (..), Yesod, mkYesod, parseRoutes,
                             redirect, warp)

-- | Our foundation datatype.
data App = App

instance Yesod App

mkYesod "App" [parseRoutes|
/         HomeR GET
/fib/#Int FibR  GET
|]

getHomeR :: Handler ()
getHomeR = redirect (FibR 1)

fibs :: [Int]
fibs = 0 : scanl (+) 1 fibs

getFibR :: Int -> Handler String
getFibR i = return $ show $ fibs !! i

main :: IO ()
main = warp 3000 App
