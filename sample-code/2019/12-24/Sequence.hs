newtype Sequence m a = Sequence { getSequence :: m a }
  deriving (Show, Eq)

instance (Monad m, Semigroup a) => Semigroup (Sequence m a) where
  ma <> mb = Sequence $
    do
      a <- getSequence ma
      b <- getSequence mb
      return (a <> b)

instance (Monad m, Monoid a) => Monoid (Sequence m a) where
  mempty = Sequence (return mempty)

e1 = [Just [1,2], Just [3,4]]
e2 = [Just [1,2], Just [3,4], Nothing]