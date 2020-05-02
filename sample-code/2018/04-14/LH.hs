module LH where

{-@ measure lLen @-}
lLen :: [a] -> Int
lLen [] = 0
lLen (_:xs) = 1 + lLen xs

{-@ type ListN a N = {v:[a] | lLen v = N} @-}

{-@ goodList :: ListN Int 2 @-}
goodList :: [Int]
goodList = [1,2]

-- {-@ badList :: ListN Int 1 @-}
badList :: [Int]
badList = [1,2]

{-@ measure nLen @-}
nLen :: [Int] -> Int
nLen [] = 0
nLen (n:ns) = n + nLen ns

{-@ type ListSum a N = {v:[a] | nLen v = N} @-}

{-@ goodListSum :: ListSum Int 111 @-}
goodListSum :: [Int]
goodListSum = [1,10,100]

-- {-@ badListSum :: ListSum Int 111 @-}
badListSum :: [Int]
badListSum = [1]