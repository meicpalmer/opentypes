{-#LANGUAGE TemplateHaskell, TypeFamilies, FlexibleContexts, InstanceSigs, MultiParamTypeClasses#-}

module Main (
    main
) where

import OpenTypes.Test.OpenInterface
import OpenTypes.Test.Fragment1
import OpenTypes.Test.Fragment2
import OpenTypes.Test.TwoParam
import OpenTypes.Finalization


-- This macro will generate a concrete definition for the nullary data family Expr.
-- It searches for visible instances of OpenSuper whose result types are Expr
-- in order to find the "fragments" of Expr defined by other modules.
-- The generated type is a series of one-argument constructors, one for each fragment type.
-- This macro will also generate Wrap instances for each of the fragment types.
finalize ''Expr

-- This macro will generate a "dispatching instance" for Eval.  It simply requires an
-- instance declaration with explicit type signatures for the functions to be generated.
-- The generated eval function will have a case for all the fragment-wrapping constructors
-- in Expr, which simply unpacks the value and calls the "eval" function on it.
-- These "eval" calls will be routing to the Eval instances defined for the fragments.
genInstance [d|
  instance Eval Expr where
    eval :: Expr -> Int
  |]

genInstance [d|
  instance Eq2 Expr Expr where
    eq2 :: Expr -> Expr -> Bool
  |]

instance Eq Expr where
  (==) a b = eq2 a b

-- Notice that this value contains a mix of constructors from Expr1 and Expr2.
ex = mult (lit 2) (add (lit 1) (lit 2))

main = do
  print (eval ex)
  print (ex == ex)
  print (ex == mult (lit 2) (add (lit 1) (lit 2)))
  print (ex == mult (lit 2) (add (lit 1) (lit 3)))
