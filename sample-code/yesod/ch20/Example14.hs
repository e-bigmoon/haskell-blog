#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package text
    --package yesod-core
-}
import           Data.Text  (pack)
import           Yesod.Core (LiteHandler, dispatchTo, dispatchTo, liteApp,
                             onStatic, redirect, warp, withDynamic)

getHomeR :: LiteHandler ()
getHomeR = redirect "/fib/1"

fibs :: [Int]
fibs = 0 : scanl (+) 1 fibs

getFibR :: Int -> LiteHandler String
getFibR i = return $ show $ fibs !! i

main :: IO ()
main = warp 3000 $ liteApp $ do
    dispatchTo getHomeR
    onStatic (pack "fib") $ withDynamic $ \i -> dispatchTo (getFibR i)