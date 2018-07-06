#!/usr/bin/env stack
-- stack script --resolver lts-11.16
import Conduit

main :: IO ()
main = print $ runConduitPure $ do
  mapM_ leftover [1..10]
  sinkList