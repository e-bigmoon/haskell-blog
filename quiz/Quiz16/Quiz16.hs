#!/usr/bin/env stack
-- stack script --resolver lts-12.2

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

intTree :: Tree Int
intTree =
  Node
    (Node
      (Leaf 1)
      (Node
        (Leaf 2)
        (Leaf 3)))
    (Node
      (Leaf 4)
      (Leaf 5))

-- | 部分木を左右反転させた木を返す
mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node l r) = Node r l

-- | 葉の数を返す
size :: Tree a -> Int
size (Leaf _) = 1
size (Node l r) = size l + size r

-- | 木の高さを返す
depth :: Tree a -> Int
depth (Leaf _) = 0
depth (Node l r) = 1 + max (depth l) (depth r)

-- | 木が平衡かどうか
isBalanced :: Tree a -> Bool
isBalanced (Leaf _) = True
isBalanced (Node l r) = diff <= 1 && isBalanced l && isBalanced r
  where
    diff = abs (size l - size r)

-- | 葉の値を集めてリストにして返す
leaves :: Tree a -> [a]
leaves (Leaf x) = [x]
leaves (Node l r) = leaves l ++ leaves r

-- | 左の部分木を返す
left :: Tree a -> Maybe (Tree a)
left (Leaf _) = Nothing
left (Node l _) = Just l

-- | 右の部分木を返す
right :: Tree a -> Maybe (Tree a)
right (Leaf _) = Nothing
right (Node _ r) = Just r

-- | 木の要素に対して関数を適用する
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

-- | 木を畳み込む
foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree f _ (Leaf x) = f x
foldTree f g (Node l r) = g (foldTree f g l) (foldTree f g r)

main :: IO ()
main = do
  print $ mirror intTree
  print $ size intTree
  print $ depth intTree
  print $ isBalanced intTree
  print $ leaves intTree
  print $ left intTree
  print $ right intTree

  print $ mapTree show intTree

  print $ foldTree id (+) intTree
  print $ foldTree id (*) intTree
  print $ foldTree (:[]) (++) intTree

{-
$ ./Quiz16.hs
Node (Node (Leaf 4) (Leaf 5)) (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
5
3
True
[1,2,3,4,5]
Just (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
Just (Node (Leaf 4) (Leaf 5))
Node (Node (Leaf "1") (Node (Leaf "2") (Leaf "3"))) (Node (Leaf "4") (Leaf "5"))
15
120
[1,2,3,4,5]
-}