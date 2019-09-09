{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Expr where

import Data.Extensible
import Data.Functor.Identity
import Data.Kind
import GHC.TypeLits

class Expr expr where
  type FieldList expr :: [Assoc Symbol Type]
  liftExpr :: Variant (FieldList expr) -> expr

matchVariant :: forall c xs r. Forall c xs => Proxy c -> (forall x. c x => Membership xs x -> TargetOf x -> r) -> Variant xs -> r
matchVariant _ f = matchField $ htabulateFor (Proxy @c) $ \m -> Field $ Match $ f m . runIdentity