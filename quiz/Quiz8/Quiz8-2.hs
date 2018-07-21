#!/usr/bin/env stack
-- stack script --resolver lts-11.3
import           Conduit
import           Data.Conduit.List

main :: IO ()
main = print $ runConduitPure $ sourceList [1..10]
                             .| sinkList
