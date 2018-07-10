#!/usr/bin/env stack
-- stack script --resolver lts-11.17
import Conduit

main:: IO ()
main = runConduit $ yieldMany [1..] .| iterMC print .| sinkNull