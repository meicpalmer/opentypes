
{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
module OpenTypes.Test.TwoParam where

import OpenTypes.Test.OpenInterface
import OpenTypes.Test.Fragment1
import OpenTypes.Test.Fragment2
import OpenTypes.Interface


deriving instance (Eq Expr) => Eq Expr1
deriving instance (Eq Expr) => Eq Expr2

instance (Eq Expr) => Eq2 Expr1 Expr1 where
  eq2 a b = (a == b)

instance (Eq Expr) => Eq2 Expr2 Expr2 where
  eq2 a b = (a == b)

-- Use the default (False) for these combinations
instance Eq2 Expr1 Expr2
instance Eq2 Expr2 Expr1
