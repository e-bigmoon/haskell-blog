#!/usr/bin/env stack
-- stack script --resolver lts-12.2

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

-- data Tree
--   = Leaf Int
--   | Node Tree Tree
