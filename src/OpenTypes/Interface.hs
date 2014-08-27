
{-#LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances#-}
module OpenTypes.Interface where

-- | This type family is used to track which types are "fragments" of which open types.
-- The finalize macro, defined in OpenTypes.Finalization, uses this type family to automatically
-- find all imported "fragments" of the given open type.
type family OpenSuper t

class Wrap a where
  -- | The wrap function simply converts from a fragment to its corresponding open type.
  wrap :: a -> OpenSuper a

  -- | The partial function takes two function arguments - a function that operates over the
  -- fragment type, and a "default" that operates over the values that are not members
  -- of this fragment.  This function exists for convenience and is emphatically NOT
  -- the mechanism for defining extensible open functions.
  -- (that is typeclasses + the genInstance macro)
  partial :: (a -> out) -> (OpenSuper a -> out) -> OpenSuper a -> out


-- | A two-parameter version of Eq.
-- Used for defining equality between different fragments of the same open type.
class Eq2 a b where
  eq2 :: a -> b -> Bool
  eq2 a b = False
