module Type.Checkerz.Lookup
       ( MonadTcLookup(..)
       , FindResult(..)) where
import GHCCompat

import Control.Monad.IO.Class (MonadIO)

type PackageName = FastString

-- | @'MonadIO'@ where one can lookup infomation of type-checking environment.
class (Monad m, MonadIO m) => MonadTcLookup m where
  findImportedModule :: ModuleName
                     -> Maybe PackageName
                     -> m FindResult
  lookupOrigin :: Module -> OccName -> m Name
  lookupGlobal :: Name -> m TyThing
