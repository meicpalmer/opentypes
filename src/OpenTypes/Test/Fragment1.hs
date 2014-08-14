
{-#LANGUAGE TypeFamilies, TemplateHaskell, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module OpenTypes.Test.Fragment1 (
  Expr1(..), lit, add, neg
) where


import OpenTypes.Test.OpenInterface
import OpenTypes.WrapConstructors


-- This is a "fragment" of the open type Expr.  It defines some constructors,
-- some of which refer to Expr itself.
data Expr1 =
    Lit Int
  | Add Expr Expr
  | Neg Expr

-- This instance definition will help the Template Haskell macros know that
-- Expr1 is a fragment of Expr.
type instance OpenSuper Expr1 = Expr


-- This macro generates a lowercase-named function for each constructor in Expr1.
-- The functions simply call the "wrap" function on the constructor expressions.
-- Their output type is Expr, rather than Expr1.
-- They have "Wrap Expr1" as a constraint - the Wrap Expr1 instance is not defined
-- yet, but the constraint indicates that the function depends on it being defined
-- at some point.
wrapCons ''Expr1

-- Notice that this value requires a signature with the "Wrap Expr1" constraint.
ex1 :: Wrap Expr1 => Expr
ex1 = add (lit 1) (lit 2)

-- This instance defines the "eval" functions over the constructors in the Expr1 fragment.
-- Notice the "Eval Expr" constraint.  Like the "Wrap Expr1" constraint, the instance
-- is not defined yet, and the constraint refers to the assumption that it will be.
-- The "recursive" eval calls in this function actually refer to the "eval" from that
-- instance.
-- A constraint like this requires UndecidableInstances and FlexibleContexts.
instance Eval Expr => Eval Expr1 where
  eval (Lit i) = i
  eval (Add a b) = eval a + eval b
  eval (Neg a) = - eval a

