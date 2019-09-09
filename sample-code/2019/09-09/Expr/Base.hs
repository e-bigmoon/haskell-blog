{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Expr.Base where

import Expr
import Field.Add
import Field.Constant
import Operation.Pretty

import Data.Extensible

newtype ExprB = ExprB
  { unwrapExprB :: Variant ExprBFields
  } deriving (Eq, Show)

type ExprBFields = '[ Constant, Add ExprB ]

instance Expr ExprB where
  type FieldList ExprB = ExprBFields
  liftExpr = ExprB

instance Pretty ExprB where
  pretty = pretty' . unwrapExprB