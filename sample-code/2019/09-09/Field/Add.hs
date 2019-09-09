{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Field.Add where

import Expr
import Operation.Pretty

import Control.Lens ((#))
import Data.Extensible

type Add expr = "add" >: (expr, expr)

instance Pretty expr => PrettyField (Add expr) where
  prettyField _ (l, r) = pretty l <> " + " <> pretty r

add e1 e2 = liftExpr (#add # (e1, e2))