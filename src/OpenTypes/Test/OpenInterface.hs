
{-#LANGUAGE TypeFamilies#-}
module OpenTypes.Test.OpenInterface where


-- A nullary data family like this indicates that the definition of the type's structure
-- is deferred to another module.  This is an important element for this encoding of
-- open types as a solution to the Expression Problem.
data family Expr


class Eval a where
  eval :: a -> Int



