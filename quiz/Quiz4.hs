module Quiz4 where

import Data.List (transpose)

dfs :: Eq a => a -> [[a]] -> Bool
dfs x = elem x . concat

bfs :: Eq a => a -> [[a]] -> Bool
bfs x = elem x . concat . transpose

f = concatMap ok [[1,2],[],[3,4]]
  where
    ok (h:_) = [h]
    ok _     = []