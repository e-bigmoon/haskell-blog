{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module Lib where

import Data.Proxy
import Data.Kind

showArgs :: Int -> Bool -> Maybe Char -> [String]
showArgs a b c = [ show a, show b, show c ]

showArgsTuple :: (Int, Bool, Maybe Char) -> (String, String, String)
showArgsTuple (a, b, c) = (show a, show b, show c)

instance Functor ((,,) a b) where
  -- fmap :: (c -> d) -> (a, b, c) -> (a, b, d)
  fmap f (a, b, c) = (a, b, f c)

-- badMapTuple3 :: (a -> r) -> (a, b, c) -> (r, r, r)
-- badMapTuple3 f (a, b, c) = (f a, f b, f c)

goodMapTuple3 :: (forall a. a -> r) -> (a, b, c) -> (r, r, r)
goodMapTuple3 f (a, b, c) = (f a, f b, f c)

mapShowTuple3 ::
  (Show a, Show b, Show c) =>
  (forall x. Show x => x -> r) -> (a, b, c) -> (r, r, r)
mapShowTuple3 f (a, b, c) = (f a, f b, f c)

mapTuple3 ::
  forall (c :: Type -> Constraint) x1 x2 x3 r.
  (c x1, c x2, c x3) =>
  Proxy c -> (forall x. c x => x -> r) -> (x1, x2, x3) -> (r, r, r)
mapTuple3 _ f (a, b, c) = (f a, f b, f c)