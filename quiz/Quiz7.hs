#!/usr/bin/env stack
-- stack script --resolver lts-11.3
import           Conduit

myTakeWhileC :: Monad m => (i -> Bool) -> ConduitM i i m ()
myTakeWhileC f = loop
  where
    loop = do
      mx <- await
      case mx of
        Nothing -> return ()
        Just x
          | f x       -> yield x >> loop
          | otherwise -> return ()

main :: IO ()
main = print $ runConduitPure $ yieldMany [1..10] .| do
  x <- myTakeWhileC (<= 5) .| sinkList
  y <- sinkList
  return (x, y)
