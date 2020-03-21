{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

class Falsy a where
  falsy :: a -> Bool

data Any = forall a. Falsy a => Any a

instance Falsy Bool where falsy = not
instance Falsy Int  where falsy = (==0)
instance Falsy Char where falsy = (==' ')
instance Falsy String where falsy = null

a1, a2, a3, a4 :: Any
a1 = Any True
a2 = Any @Int 0 
a3 = Any 'a'
a4 = Any "haskell"

list :: [Any]
list = [a1, a2, a3, a4]

elimAny :: Any -> Bool
elimAny = \case
  Any x -> falsy x