{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Expr.Mul where

import Expr
import Expr.Base
import Field.Mul
import Operation.Pretty

import Data.Extensible

newtype ExprM = ExprM
  { unwrapExprM :: Variant ExprMFields
  } deriving (Eq, Show)

type ExprMFields = ExprBFields ++ '[ Mul ExprM ]
-- type ExprMFields = '[ Constant, Add ExprM, Mul ExprM ]

instance Expr ExprM where
  type FieldList ExprM = ExprMFields
  liftExpr = ExprM

instance Pretty ExprM where
  pretty = pretty' . unwrapExprM