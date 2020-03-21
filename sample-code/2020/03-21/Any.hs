{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
data Any = forall a. Any a (a -> Bool)

a1, a2, a3, a4 :: Any
a1 = Any True not
a2 = Any 0 (==0)
a3 = Any 'a' (==' ')
a4 = Any "haskell" null

list :: [Any]
list = [a1, a2, a3, a4]

elimAny :: Any -> Bool
elimAny = \case
  Any x f -> f x