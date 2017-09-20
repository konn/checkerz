{-# LANGUAGE DeriveFunctor, FlexibleInstances, GADTs                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, LiberalTypeSynonyms        #-}
{-# LANGUAGE MultiParamTypeClasses, ParallelListComp, RankNTypes    #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TupleSections #-}
module Type.Checkerz
    ( IsConstraint(..)
    , Simplifier
    , compilePlugin
    , Constr
    , Parser
    , constraint
    , redundant
    , derived
    , inaccessible
    , toSimplify
    ) where
import GHCCompat

import           Control.Monad.Except
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.State       (MonadState (..))
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Control.Monad.Trans.RWS   (RWST, ask, execRWST, tell)
import qualified Data.IntMap.Strict        as IM
import           Data.IORef                (IORef, atomicModifyIORef', newIORef)
import           Data.IORef                (readIORef, writeIORef)
import           Data.Maybe                (catMaybes, mapMaybe, maybeToList)
import           Data.Tuple                (swap)

type ConstrId = Int

newtype Simplifier s c a =
  Simplifier { runSimplifier :: RWST (IORef s) ([ConstrId], [c]) () (ExceptT [ConstrId] TcPluginM) a }
  deriving (Functor, Applicative, Monad)

data Constr a = Constr { constrId   :: Int
                       , constraint :: a
                       } deriving (Functor)

instance MonadIO (Simplifier s c) where
  liftIO = Simplifier . lift . lift . tcPluginIO

instance MonadState s (Simplifier s c) where
  get = liftIO . readIORef =<< Simplifier ask
  put s = liftIO . flip writeIORef s =<< Simplifier ask
  state f = liftIO . flip atomicModifyIORef' (swap . f) =<< Simplifier ask

newtype Parser a = Parser { runParser :: TcPluginM a }
                 deriving (Functor, Applicative, Monad)

instance MonadIO Parser where
  liftIO = Parser . tcPluginIO

class IsConstraint constr where
  fromCt :: Ct -> Parser (Maybe constr)
  toCt   :: constr -> Parser Ct

-- | Mark the given constraint as redundant (solved).
redundant :: Constr c -> Simplifier s c ()
redundant c = Simplifier $ tell ([constrId c], [])

-- | Assert a new constraint derived from given constraints.
derived :: c -> Simplifier s c ()
derived c = Simplifier $ tell ([], [c])

-- | Abort simplification with contradicting subset of given constraints.
inaccessible :: [Constr c] -> Simplifier s c a
inaccessible = Simplifier . lift . throwError . map constrId

toSimplify :: IsConstraint c
           => ([Constr c] -> Simplifier s c ())
           -> s
           -> [Ct]
           -> TcPluginM TcPluginResult
toSimplify simpl s0 cts = do
  targs <- catMaybes <$> mapM (\ct -> fmap (ct,) <$> runParser (fromCt ct)) cts
  let (cs, dic0) = unzip [(Constr i c, (i, ct))
                         | (ct, c) <- targs
                         | i <- [0..]
                         ]
      dic = IM.fromList dic0
  ref <- tcPluginIO $ newIORef s0
  ans <- runExceptT $ execRWST (runSimplifier $ simpl cs) ref ()
  case ans of
    Left gs -> return $ TcPluginContradiction $ mapMaybe (`IM.lookup` dic)  gs
    Right (_, (ids, ds)) -> do
      let solvs = map (undefined, ) $ mapMaybe (`IM.lookup` dic) ids
      derivs <- mapM (runParser . toCt) ds
      return $ TcPluginOk solvs derivs

compilePlugin :: IsConstraint constr
              => constr
              -> TcPlugin
compilePlugin _ = undefined
