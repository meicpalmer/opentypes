
{-#LANGUAGE TypeFamilies, TemplateHaskell, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module OpenTypes.Test.Fragment2 (
  Expr2(..), mult, sub
) where

import OpenTypes.Test.OpenInterface
import OpenTypes.WrapConstructors

-- See module TestFragment1 for information about what's going on here.
-- This module is defining a fragment for Expr and an Eval operation over the fragment's cases,
-- and it does so entirely independenly from TestFragment1 (it does not even import it.)

data Expr2 =
    Mult Expr Expr
  | Sub Expr Expr

instance Eval Expr => Eval Expr2 where
  eval (Mult a b) = eval a * eval b
  eval (Sub a b) = eval a - eval b

type instance OpenSuper Expr2 = Expr

wrapCons ''Expr2


