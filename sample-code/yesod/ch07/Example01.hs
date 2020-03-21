#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package text
    --package yesod
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import qualified Data.Text as T (pack, unpack)
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/             HomeR   GET
/page/faq     FaqR    GET
/fib/#Natural FibR    GET
|]

instance Yesod App

newtype Natural = Natural Int
  deriving (Eq, Show, Read)

instance PathPiece Natural where
  toPathPiece (Natural i) = T.pack $ show i
  fromPathPiece s =
    case reads $ T.unpack s of
      (i, ""):_
        | i < 1 -> Nothing
        | otherwise -> Just $ Natural i
      [] -> Nothing

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|HOME|]

getFaqR :: Handler Html
getFaqR = defaultLayout [whamlet|FAQ|]

getFibR :: Natural -> Handler Html
getFibR (Natural n) = defaultLayout [whamlet|fib(#{show n}) = #{fib n}|]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = warp 3000 App
