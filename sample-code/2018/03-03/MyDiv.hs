module MyDiv where
{-@ type NonZero = {v:Int | v /= 0} @-}

{-@ myDiv :: Int -> NonZero -> Int @-}
myDiv :: Int -> Int -> Int
myDiv = div

good :: Int
good = myDiv 10 2

bad :: Int
bad = myDiv 10 0