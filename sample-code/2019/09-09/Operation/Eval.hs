{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Operation.Eval where

import Expr
import Expr.Base
import Expr.Mul
import Field.Add
import Field.Constant
import Field.Mul

import Data.Extensible

eval' :: Forall EvalField xs => Variant xs -> Int
eval' = matchVariant (Proxy @EvalField) evalField

class Eval expr where
  eval :: expr -> Int

instance Eval ExprB where
  eval = eval' . unwrapExprB

instance Eval ExprM where
  eval = eval' . unwrapExprM

class EvalField kv where
  evalField :: proxy kv -> TargetOf kv -> Int

instance EvalField Constant where
  evalField _ = id

instance Eval expr => EvalField (Add expr) where
  evalField _ (l, r) = eval l + eval r

instance Eval expr => EvalField (Mul expr) where
  evalField _ (l, r) = eval l * eval r
