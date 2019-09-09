{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Field.Constant where

import Expr
import Operation.Pretty

import Control.Lens ((#))
import Data.Extensible

type Constant = "constant" >: Int

instance PrettyField Constant where
  prettyField _ = show

c i = liftExpr (#constant # i)