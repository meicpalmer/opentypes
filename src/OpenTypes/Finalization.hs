{-#LANGUAGE TemplateHaskell, DoAndIfThenElse, FlexibleInstances#-}


module OpenTypes.Finalization (
  finalize, genInstance, Eq2(..)
) where

import Language.Haskell.TH
import Data.Maybe (mapMaybe, fromMaybe)
import OpenTypes.Interface
import qualified Data.Map as M


findFragments name kind = do
  info <- reify ''OpenSuper
  (norm, varlist) <- normalizeVars name kind
  case info of
    (FamilyI _ instances) ->
      let checkInstance inst = case inst of
            TySynInstD _ [one] out -> do
              umap <- unifyVarnames out norm
              return (transformVarnames umap one)
          fragments = mapMaybe checkInstance instances
      in if null fragments
      then fail $ "No fragment types found for "++show name
      else return (fragments, varlist)

unifyVarnames :: Type -> Type -> Maybe (M.Map Name Name)
unifyVarnames a b = case (a, b) of
  (AppT a1 a2, AppT b1 b2) -> do
    m1 <- unifyVarnames a1 b1
    m2 <- unifyVarnames a2 b2
    return (M.union m1 m2)
  (VarT a1, VarT b1) -> return (M.singleton a1 b1)
  (SigT a1 ak, b) -> unifyVarnames a1 b
  (a, SigT b1 bk) -> unifyVarnames a b1
  (a, b) ->
    if a == b
    then return M.empty
    else Nothing

transformVarnames :: M.Map Name Name -> Type -> Type
transformVarnames m t = case t of
  VarT n -> case M.lookup n m of
    Just res -> VarT res
    Nothing -> VarT n
  AppT a b -> AppT (transformVarnames m a) (transformVarnames m b)
  other -> other

normalizeVars name kind = let
    varlist StarT = return []
    varlist (AppT (AppT ArrowT a) b) = do
      n1 <- newName "v"
      vs <- varlist b
      return $ (VarT n1) : vs
    makeT acc [] = acc
    makeT acc (h : t) = makeT (AppT acc h) t
  in do
    lst <- varlist kind
    return $ (makeT (ConT name) lst, lst)

firstTypeName (ConT name) = name
firstTypeName (AppT a b) = firstTypeName a

makeConstructorName t = let
    fname = firstTypeName t
  in case nameModule fname of
    Just m -> concat [m, "__", nameBase fname]
    Nothing -> nameBase fname


finalize name = do
  info <- reify name
  case info of
    FamilyI (FamilyD DataFam _ [] k) instances -> do
      (fragments, varlist) <- findFragments name (fromMaybe StarT k)
      connames <- mapM (\t -> newName ("Con__"++makeConstructorName t)) fragments
      let consAndTypes = zip connames fragments
      let genWrapInstance (n, t) = [d|
            instance Wrap $(return t) where
              wrap = $(return $ ConE n)
              partial f def val = $( do
                  f <- [|f|]
                  def <- [|def|]
                  val <- [|val|]
                  return $ CaseE val [   -- This is an ugly workaround for the fact that TH does not implement pattern splices
                    let vname = mkName "v" in
                      Match (ConP n [VarP vname]) (NormalB $ AppE f (VarE vname)) [],
                    let othername = mkName "other" in
                      Match (VarP othername) (NormalB $ AppE def (VarE othername)) []
                    ] )
           |]
      wrapinstances <- mapM genWrapInstance consAndTypes
      let fragmentcon (n, t) = NormalC n [(IsStrict, t)]
      let cons = map fragmentcon consAndTypes
      let datainstance = DataInstD [] name varlist cons []
      return $ datainstance : concat wrapinstances

opencons :: Type -> Q (Maybe [(Name, Type)])
opencons (ConT name) = do
  info <- reify name
  case info of
    FamilyI (FamilyD DataFam _ [] _) instances -> case instances of
      [] -> fail $ "No instance for "++show name
      [DataInstD _ _ _ cons _] -> return $ Just $ do
        (NormalC n [(s, t)]) <- cons
        return (n, t)
      more -> fail $ "Multiple instances for "++show name
    other -> return Nothing
opencons (AppT a b) = opencons a
opencons (SigT a k) = opencons a
opencons (ForallT _ _ a) = opencons a
opencons other = return Nothing

openpatterns :: Type -> Q [(Pat, Exp)]
openpatterns t = do
  consop <- opencons t
  case consop of
    Just pairs ->
      mapM (\(n, t) -> do{
        vname <- newName "v";
        return (ConP n [VarP vname], VarE vname)}) pairs
    Nothing -> do
      vname <- newName "v"
      return [(VarP vname, VarE vname)]


genDispatchClauses name intypes = let
      dispatchOne :: Exp -> [(Pat, Exp)] -> [Clause]
      dispatchOne f patterns = do
        (pat, var) <- patterns
        return $ Clause [pat] (NormalB (AppE f var)) []
      dispatchMany f patternlists = case patternlists of
        [one] -> dispatchOne f one
        (h : t) -> do
          Clause hpats (NormalB hbody) _ <- dispatchOne f h
          Clause tpats tbody _ <- dispatchMany hbody t
          return $ Clause (hpats ++ tpats) tbody []
    in do
      patlists <- mapM openpatterns intypes
      let clauses = dispatchMany (VarE name) patlists
      return $ FunD name clauses

genOneDispatch :: Dec -> Q Dec
genOneDispatch dec = case dec of
    SigD name t -> let
        inputTypes (AppT (AppT ArrowT a) b) = a : inputTypes b
        inputTypes other = []
      in genDispatchClauses name (inputTypes t)
    other -> return other

genOneInstance instancedec = case instancedec of
  InstanceD ctx t decs -> do
    newdecs <- mapM genOneDispatch decs
    return $ InstanceD ctx t newdecs
  other -> fail $ (pprint other) ++ " is not an instance declaration."

genInstance instancedecs = do
  decs <- instancedecs
  mapM genOneInstance decs
