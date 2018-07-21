#!/usr/bin/env stack
-- stack script --resolver lts-11.0
import           Conduit

sink :: Monad m => ConduitM Int o m (String, Int)
sink = do
  x <- takeC 5 .| mapC show .| foldC
  y <- sumC
  return (x, y)

main :: IO ()
main = do
  let res = runConduitPure $ yieldMany [1..10] .| sink
  print res
