#!/usr/bin/env stack
-- stack script --resolver lts-12.7

data Person = Person
  { personName :: String
  , personAge  :: Int
  } deriving Show

f :: Person
f = Person { personName = "bigmoon" }
