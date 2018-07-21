#!/usr/bin/env stack
-- stack script --resolver lts-12.0
import           Conduit

main :: IO ()
main = runConduit $ yieldMany [1..10] .| iterMC print .| return () .| sinkNull
