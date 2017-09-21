{-# LANGUAGE CPP, DeriveDataTypeable, PatternGuards, PatternSynonyms #-}
module GHCCompat (module GHCCompat) where
import Coercion            as GHCCompat (coercionType)
import FastString          as GHCCompat (fsLit)
import FastString          as GHCCompat (FastString)
import GHC.TcPluginM.Extra as GHCCompat (evByFiat, lookupModule, lookupName,
                                         tracePlugin)
import GhcPlugins          as GHCCompat (EqRel (..), PredTree (..))
import GhcPlugins          as GHCCompat (classifyPredType, isEqPred, mkTyConTy)
import GhcPlugins          as GHCCompat (mkTcOcc, ppr, promotedFalseDataCon)
import GhcPlugins          as GHCCompat (promotedTrueDataCon, text)
import GhcPlugins          as GHCCompat (tyConAppTyCon_maybe, typeKind)
import GhcPlugins          as GHCCompat (TyCon, typeNatKind)
import Module              as GHCCompat (ModuleName, mkModuleName)
import Module              as GHCCompat (Module)
import Name                as GHCCompat (Name)
import OccName             as GHCCompat (OccName)
import Plugins             as GHCCompat (Plugin (..), defaultPlugin)
import TcEvidence          as GHCCompat (EvTerm)
import TcPluginM           as GHCCompat (tcLookupTyCon)
import TcPluginM           as GHCCompat (FindResult (..), TcPluginM,
                                         tcLookupTyCon, tcPluginIO,
                                         tcPluginTrace)
import TcRnMonad           as GHCCompat (Ct, TcPluginResult (..), isWanted)
import TcRnTypes           as GHCCompat (TcPlugin (..), ctEvPred, ctEvidence)
import TcTypeNats          as GHCCompat
import TyCon               as GHCCompat (tyConName)
import Type                as GHCCompat (TyThing (..))
#if __GLASGOW_HASKELL__ >= 800
import           GhcPlugins (InScopeSet, Outputable, emptyUFM)
import qualified PrelNames  as Old
import           TyCoRep    as GHCCompat (TyLit (NumTyLit, StrTyLit), Type (..))
import           Type       as GHCCompat (TCvSubst (..), TvSubstEnv,
                                          emptyTCvSubst)
import           Type       as GHCCompat (eqType, unionTCvSubst)
import qualified Type       as Old
import           TysWiredIn as GHCCompat (boolTyCon)
import           Unify      as Old (tcUnifyTy)
#if __GLASGOW_HASKELL__ >= 802
import Var as GHCCompat (ArgFlag (..), TyVarBndr (..))
#else
import TyCoRep as GHCCompat (TyBinder (..), VisibilityFlag (..))
#endif
#else
import Data.Data  (Data)
import Type       as GHCCompat (TvSubst, emptyTvSubst)
import Type       as GHCCompat (substTy, unionTvSubst)
import TypeRep    as GHCCompat (TyLit (NumTyLit, StrTyLit), Type (..))
import TysWiredIn as Old (eqTyCon)
import TysWiredIn as GHCCompat (promotedBoolTyCon)
import Unify      as GHCCompat (tcUnifyTy)
import Var        (Var)
#endif
import TcPluginM (lookupOrig)
import Type      as GHCCompat (splitTyConApp_maybe)
#if __GLASGOW_HASKELL__ < 802
import           Data.Data (Data)
import           Type      as GHCCompat (isVoidTy)
import qualified Var       as Var
#else
import RepType as GHCCompat (isVoidTy)
#endif
import Unique as GHCCompat (getKey, getUnique)

#if __GLASGOW_HASKELL__ >= 800
data TvSubst = TvSubst InScopeSet TvSubstEnv

instance Outputable  TvSubst where
  ppr = ppr . toTCv

emptyTvSubst :: TvSubst
emptyTvSubst = case emptyTCvSubst of
  TCvSubst set tvsenv _ -> TvSubst set tvsenv

toTCv :: TvSubst -> TCvSubst
toTCv (TvSubst set tvenv) = TCvSubst set tvenv emptyUFM

substTy :: TvSubst -> Type -> Type
substTy tvs = Old.substTy (toTCv tvs)

unionTvSubst :: TvSubst -> TvSubst -> TvSubst
unionTvSubst s1 s2 =
  fromTCv $ unionTCvSubst (toTCv s1) (toTCv s2)
fromTCv :: TCvSubst -> TvSubst
fromTCv (TCvSubst set tvsenv _) = TvSubst set tvsenv

promotedBoolTyCon :: TyCon
promotedBoolTyCon = boolTyCon

#if __GLASGOW_HASKELL__ == 800
pattern FunTy :: Type -> Type -> Type
pattern FunTy t1 t2 = ForAllTy (Anon t1) t2
#endif

tcUnifyTy :: Type -> Type -> Maybe TvSubst
tcUnifyTy t1 t2 = fromTCv <$> Old.tcUnifyTy t1 t2

getEqTyCon :: TcPluginM TyCon
getEqTyCon = tcLookupTyCon Old.eqTyConName

#else
eqType :: Type -> Type -> Bool
eqType = (==)

getEqTyCon :: TcPluginM TyCon
getEqTyCon = return Old.eqTyCon
#endif

#if __GLASGOW_HASKELL__ < 802
type TyVar = Var.Var
data TyVarBndr tyvar argf = TvBndr tyvar argf
                          deriving (Data)
#if __GLASGOW_HASKELL__ < 800
data ArgFlag = Required	| Specified	| Inferred
             deriving (Eq, Data)
type TyVarBinder = TyVarBndr TyVar ArgFlag
#endif
#endif


getEqWitnessTyCon :: TcPluginM TyCon
getEqWitnessTyCon = do
  md <- lookupModule (mkModuleName "Data.Type.Equality") (fsLit "base")
  tcLookupTyCon =<< lookupOrig md (mkTcOcc ":~:")
