{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Operation.Pretty where

import Expr

import Data.Extensible

pretty' :: Forall PrettyField xs => Variant xs -> String
pretty' = matchVariant (Proxy @PrettyField) prettyField

class Expr expr => Pretty expr where
  pretty :: expr -> String

class PrettyField kv where
  prettyField :: proxy kv -> TargetOf kv -> String
