
{-# LANGUAGE TypeFamilies #-}
-- This module is in the project, but not imported by Main or any other module.
-- It declares a bad finalization of Expr which only includes Expr1.
-- This is designed to test whether Haskell will bring the bad instance into scope.
-- It should not.

-- After testing, it does not, which is good.

module OpenTypes.Test.BadModule where

import OpenTypes.Test.OpenInterface
import OpenTypes.Test.Fragment1


data instance Expr = C Expr1





