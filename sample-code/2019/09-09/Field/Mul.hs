{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Field.Mul where

import Expr
import Operation.Pretty

import Control.Lens ((#))
import Data.Extensible

type Mul expr = "mul" >: (expr, expr)

instance Pretty expr => PrettyField (Mul expr) where
  prettyField _ (l, r) = pretty l <> " * " <> pretty r

mul e1 e2 = liftExpr (#mul # (e1, e2))