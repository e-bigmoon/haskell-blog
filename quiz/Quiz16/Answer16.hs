#!/usr/bin/env stack
-- stack script --resolver lts-12.2

{-# LANGUAGE InstanceSigs #-}

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Data.Maybe

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    v <- choose (0 :: Int, 1)
    if v == 0
      then Leaf <$> arbitrary
      else Node <$> arbitrary <*> arbitrary
  shrink (Leaf _) = []
  shrink (Node l r) =
    [l, r] ++ [ Node l' r' | (l', r') <- shrink (l, r) ]

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
mirror (Leaf x)   = Leaf x
mirror (Node l r) = Node (mirror r) (mirror l)

-- | 木の高さを返す
depth :: Tree a -> Int
depth (Leaf _)   = 0
depth (Node l r) = 1 + max (depth l) (depth r)

-- | 木が平衡かどうか
isBalanced :: Tree a -> Bool
isBalanced (Leaf _) = True
isBalanced (Node l r) = diff <= 1 && isBalanced l && isBalanced r
  where
    diff = abs (depth l - depth r)

isBalanced' :: Tree a -> Bool
isBalanced' = check . until (or . fmap isLeaf) dig . pure
  where
    dig :: [Tree a] -> [Tree a]
    dig = concatMap step

    isLeaf :: Tree a -> Bool
    isLeaf (Leaf _) = True
    isLeaf _ = False

    check :: [Tree a] -> Bool
    check = null . dig . dig

    step :: Tree a -> [Tree a]
    step (Leaf _) = []
    step (Node l r) = [l, r]

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x)   = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f e (Leaf x)   = f x e
  foldr f e (Node l r) = foldr f (foldr f e r) l

main :: IO ()
main = do
  print $ mirror intTree
  print $ depth intTree
  print $ isBalanced intTree

  print $ fmap show intTree

  print $ foldr (+) 0 intTree
  print $ foldr (*) 1 intTree

{-
$ ./Answer16.hs
Node (Node (Leaf 5) (Leaf 4)) (Node (Node (Leaf 3) (Leaf 2)) (Leaf 1))
3
True
Node (Node (Leaf "1") (Node (Leaf "2") (Leaf "3"))) (Node (Leaf "4") (Leaf "5"))
15
120
-}
