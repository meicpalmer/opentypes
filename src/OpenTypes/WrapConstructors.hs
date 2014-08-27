{-#LANGUAGE TemplateHaskell#-}
module OpenTypes.WrapConstructors (
  wrapCons, OpenSuper, Wrap
) where

import Language.Haskell.TH
import OpenTypes.Interface
import Data.Char (toLower)

binderToType (PlainTV name) = VarT name
binderToType (KindedTV name k) = VarT name

appMany :: (a -> a -> a) -> a -> [a] -> a
appMany appf f [] = f
appMany appf f (h : t) = appMany appf (appf f h) t

appManyExpr f params = appMany AppE f params
appManyType f params = appMany AppT f params

arrows [] out = out
arrows (h : t) out = AppT (AppT ArrowT h) (arrows t out)


lowercasename name = mkName (lowercasefirst (nameBase name)) where
  lowercasefirst (h:t) = toLower h : t
  lowercasefirst [] = []



wrapconstraint t = ClassP ''Wrap t

makeArbitraryParams list = mapM (\i -> newName "p") list

-- | This macro takes the name of a data type as its parameter.
-- It generates functions whose names are lower-case versions of the names of its
-- data constructors, and the same parameter types.  These functions simply call
-- "wrap" on a value constructed from the corresponding constructor.  The functions
-- also have an explicit "Wrap X" constraint in their type signatures, where X is the
-- name of the type passed to this macro.  This Wrap constraint indicates the assumption
-- that the Wrap instance in question will be defined at some point in some other module.
wrapCons dataname = do
  info <- reify dataname
  case info of
    TyConI (DataD ctx name binders cons _) -> let
        datatype = appManyType (ConT name) (map binderToType binders)
        wrapconstraint = ClassP ''Wrap [datatype]
        wrapfunction conname params = FunD funcname [Clause (map VarP params) body []] where
          funcname = lowercasename conname
          conscall = appManyExpr (ConE conname) (map VarE params)
          wrapcall = AppE (VarE 'wrap) conscall
          body = NormalB wrapcall
        funcsig name paramtypes =
          SigD (lowercasename name) $ (ForallT [] [wrapconstraint] ftype) where
            ftype = arrows paramtypes (AppT (ConT ''OpenSuper) datatype)
        wrapcon (NormalC name sts) = do
          params <- makeArbitraryParams sts
          return [funcsig name (map (\(s, t) -> t) sts), wrapfunction name params]
        wrapcon (RecC name vsts) = do
          params <- makeArbitraryParams vsts
          return [funcsig name (map (\(v, s, t) -> t) vsts), wrapfunction name params]
        wrapcon (ForallC _ _ con) = wrapcon con
        wrapcon (InfixC _ _ _) = do
          reportWarning "Cannot automatically generate wrapped version of an Infix Constructor."
          return []
      in fmap concat $ mapM wrapcon cons
    other -> do
      reportError $ "Expected the name of a data-type, but got "++ pprint other
      return []

