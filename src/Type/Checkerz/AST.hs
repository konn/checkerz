{-# LANGUAGE CPP, EmptyCase, PatternSynonyms #-}
module Type.Checkerz.AST
       ( module Name
       , module OccName
       , module Var
       , TyLit
       , Type
       , Pred
       , ghcTypeToType
       , typeToGHCType
       , ctToPred
       ) where
import qualified Class      as GHC
import           FastString (fsLit, unpackFS)
import qualified GHCCompat  as GHC
import           Name
import           OccName
import           Var        (Var)


data TyLit = Symbol  String
           | Natural Integer
           deriving (Read, Show, Eq, Ord)

data Type = VarTy Var
          | ConTy Name [Type]
          | AppsTy Type [Type]
          | LitTy TyLit
          | FunTy Type Type
          | Forall Var Type
          deriving (Eq, Ord)

data Pred = Type :~  Type
          | Class Name [Type]
          deriving (Eq, Ord)

ctToPred :: GHC.Ct -> Maybe Pred
ctToPred ct =
  case GHC.classifyPredType $ GHC.ctEvPred $ GHC.ctEvidence ct of
    GHC.ClassPred cls args -> Just $ Class (GHC.className cls) (map ghcTypeToType args)
    GHC.EqPred _ t u -> Just $ ghcTypeToType t :~ ghcTypeToType u
    _ -> Nothing

ghcTypeToType :: GHC.Type -> Type
ghcTypeToType (GHC.TyVarTy v)     = VarTy v
ghcTypeToType (GHC.TyConApp t ts) = ConTy (GHC.tyConName t) $ map ghcTypeToType ts
ghcTypeToType (GHC.AppTy v u)     = stripApps (GHC.AppTy v u)
#if __GLASGOW_HASKELL__ >= 802
ghcTypeToType (GHC.ForAllTy (GHC.TvBndr tv _) t)  = Forall tv (ghcTypeToType t)
#elif __GLASGOW_HASKELL__ >= 800
ghcTypeToType (GHC.ForAllTy ~(GHC.Named tv _) t)  = Forall tv (ghcTypeToType t)
#else
ghcTypeToType (GHC.ForAllTy tv t)  = Forall tv (ghcTypeToType t)
#endif
ghcTypeToType (GHC.FunTy l r)     = FunTy (ghcTypeToType l) (ghcTypeToType r)
ghcTypeToType (GHC.LitTy l)       = LitTy $ ghcLitToTyLit l
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
ghcTypeToType (GHC.CastTy c _)    = ghcTypeToType c
ghcTypeToType (GHC.CoercionTy c)  = ghcTypeToType $ GHC.coercionType c
#endif

typeToGHCType :: Type -> GHC.TcPluginM GHC.Type
typeToGHCType (VarTy v)    = return $ GHC.TyVarTy v
typeToGHCType (LitTy l)    = return $ GHC.LitTy (litToGHCLit l)
typeToGHCType (FunTy l r)  = GHC.FunTy <$> typeToGHCType l <*> typeToGHCType r
typeToGHCType (Forall v t) =
#if __GLASGOW_HASKELL__ >= 802
  GHC.ForAllTy (GHC.TvBndr v GHC.Specified) <$> typeToGHCType t
#elif __GLASGOW_HASKELL__ >= 800
  GHC.ForAllTy (GHC.Named v GHC.Visible) <$> typeToGHCType t
#else
  GHC.ForAllTy v <$> typeToGHCType t
#endif
typeToGHCType (ConTy n ts) = do
  con <- GHC.tcLookupTyCon n
  GHC.TyConApp con <$> mapM typeToGHCType ts
typeToGHCType (AppsTy t ts) = do
  us <- mapM typeToGHCType (t : ts)
  return $ foldl1 GHC.AppTy us

stripApps :: GHC.Type -> Type
stripApps = loop []
  where
    loop acc (GHC.AppTy l r) = loop (ghcTypeToType r : acc) l
    loop acc t               = AppsTy (ghcTypeToType t) acc

litToGHCLit :: TyLit -> GHC.TyLit
litToGHCLit (Natural i) = GHC.NumTyLit i
litToGHCLit (Symbol s)  = GHC.StrTyLit $ fsLit s

ghcLitToTyLit :: GHC.TyLit -> TyLit
ghcLitToTyLit (GHC.NumTyLit i) = Natural i
ghcLitToTyLit (GHC.StrTyLit s) = Symbol $ unpackFS s

