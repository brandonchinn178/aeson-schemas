{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module TestUtils.MockQ
  ( MockQ(..)
  , runMockQ
  , emptyMockQ
  ) where

import Control.Monad.Except
    (ExceptT, MonadError, catchError, runExceptT, throwError)
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Time as Time
import Language.Haskell.TH (Q, runQ)
import Language.Haskell.TH.Syntax (Quasi(..), mkNameU)
import System.IO.Unsafe (unsafePerformIO)

-- | An implementation of Quasi.
data MockQ = MockQ
  { runIOUnsafe :: Bool
    -- ^ If True, run IO actions in unsafePerformIO. Otherwise, IO actions are forbidden.
  }

emptyMockQ :: MockQ
emptyMockQ = MockQ
  { runIOUnsafe = False
  }

runMockQ :: MockQ -> Q a -> Either String a
runMockQ mockQ = runIdentity . runExceptT . (`runReaderT` mockQ) . unMockQMonad . runQ

newtype MockQMonad a = MockQMonad { unMockQMonad :: ReaderT MockQ (ExceptT String Identity) a }
  deriving (Functor, Applicative, Monad, MonadReader MockQ, MonadError String)

instance MonadIO MockQMonad where
  liftIO io = do
    MockQ{runIOUnsafe} <- ask
    if runIOUnsafe
      then return $ unsafePerformIO io
      else error "IO actions not allowed"

instance MonadFail MockQMonad where
  fail = throwError

instance Quasi MockQMonad where
  qNewName name = return $ unsafePerformIO $ do
    Time.UTCTime day time <- Time.getCurrentTime
    let n = Time.toModifiedJulianDay day + Time.diffTimeToPicoseconds time
    return $ mkNameU name $ fromInteger n

  qRecover (MockQMonad handler) (MockQMonad action) = MockQMonad $ action `catchError` const handler
  qReport b msg = unsafePerformIO (qReport b msg) `seq` return ()

  qLookupName = error "Cannot run 'qLookupName' using runMockQ"
  qReify = error "Cannot run 'qReify' using runMockQ"
  qReifyFixity = error "Cannot run 'qReifyFixity' using runMockQ"
  qReifyInstances = error "Cannot run 'qReifyInstances' using runMockQ"
  qReifyRoles = error "Cannot run 'qReifyRoles' using runMockQ"
  qReifyAnnotations = error "Cannot run 'qReifyAnnotations' using runMockQ"
  qReifyModule = error "Cannot run 'qReifyModule' using runMockQ"
  qReifyConStrictness = error "Cannot run 'qReifyConStrictness' using runMockQ"
  qLocation = error "Cannot run 'qLocation' using runMockQ"
  qRunIO = error "Cannot run 'qRunIO' using runMockQ"
  qAddDependentFile = error "Cannot run 'qAddDependentFile' using runMockQ"
  qAddTopDecls = error "Cannot run 'qAddTopDecls' using runMockQ"
  qAddModFinalizer = error "Cannot run 'qAddModFinalizer' using runMockQ"
  qGetQ = error "Cannot run 'qGetQ' using runMockQ"
  qPutQ = error "Cannot run 'qPutQ' using runMockQ"
  qIsExtEnabled = error "Cannot run 'qIsExtEnabled' using runMockQ"
  qExtsEnabled = error "Cannot run 'qExtsEnabled' using runMockQ"

#if MIN_VERSION_template_haskell(2,13,0)
  qAddCorePlugin = error "Cannot run 'qAddCorePlugin' using runMockQ"
#endif

#if MIN_VERSION_template_haskell(2,14,0)
  qAddTempFile = error "Cannot run 'qAddTempFile' using runMockQ"
  qAddForeignFilePath = error "Cannot run 'qAddForeignFilePath' using runMockQ"
#elif MIN_VERSION_template_haskell(2,12,0)
  qAddForeignFile = error "Cannot run 'qAddForeignFile' using runMockQ"
#endif

#if MIN_VERSION_template_haskell(2,16,0)
  qReifyType = error "Cannot run 'qReifyType' using runMockQ"
#endif
