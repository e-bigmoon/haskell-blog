#!/usr/bin/env stack
-- stack script --resolver lts-11.3
import Conduit

main :: IO ()
main = print $ runConduitPure $ return () .| do
  mapM_ leftover [1..10]
  sinkList