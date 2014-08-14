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

