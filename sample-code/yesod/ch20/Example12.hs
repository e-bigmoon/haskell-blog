#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package wai
    --package yesod-core
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
import           Network.Wai (pathInfo)
import           Yesod.Core  (HandlerT, RenderRoute (..),
                              Yesod,
                              YesodDispatch (..), toPathPiece, fromPathPiece, redirect, notFound, yesodRunner, warp)

-- | Our foundation datatype.
data App = App

instance Yesod App

instance RenderRoute App where
    data Route App = HomeR | FibR Int
        deriving (Show, Read, Eq, Ord)

    renderRoute HomeR = ([], [])
    renderRoute (FibR i) = (["fib", toPathPiece i], [])

parseRoute' [] = Just HomeR
parseRoute' ["fib", i] = FibR <$> fromPathPiece i
parseRoute' _ = Nothing


instance YesodDispatch App where
    yesodDispatch yesodRunnerEnv req sendResponse =
        let maybeRoute = parseRoute' (pathInfo req)
            handler =
                case maybeRoute of
                    Nothing -> notFound
                    Just HomeR -> getHomeR
                    Just (FibR i) -> getFibR i
         in yesodRunner handler yesodRunnerEnv maybeRoute req sendResponse

getHomeR = redirect (FibR 1)

fibs :: [Int]
fibs = 0 : scanl (+) 1 fibs

getFibR i = return $ show $ fibs !! i

main :: IO ()
main = warp 3000 App
