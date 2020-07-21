{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TestUtils.MockQ
  ( MockQ(..)
  , runMockQ
  , runMockQErr
  , tryMockQ
  , emptyMockQ
  , loadNames
  ) where

import Control.Monad (when)
import Control.Monad.Except
    (ExceptT, MonadError, catchError, runExceptT, throwError)
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Maybe (fromMaybe)
import qualified Data.Time as Time
import Language.Haskell.TH (ExpQ, Info, Name, Q, listE, reify, runQ)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax (Lift(..), Quasi(..), mkNameU)
import System.IO.Unsafe (unsafePerformIO)

-- | An implementation of Quasi.
data MockQ = MockQ
  { runIOUnsafe :: Bool
    -- ^ If True, run IO actions in unsafePerformIO. Otherwise, IO actions are forbidden.
  , knownNames  :: [(String, Name)]
    -- ^ Names that can be looked up with `lookupName`/`lookupTypeName`/`lookupValueName`
  , reifyInfo   :: [(Name, Info)]
    -- ^ Info for Names that can be reified. Use `loadNames`.
  } deriving (Lift)

emptyMockQ :: MockQ
emptyMockQ = MockQ
  { runIOUnsafe = False
  , knownNames = []
  , reifyInfo = []
  }

loadNames :: [Name] -> ExpQ
loadNames names = listE $ flip map names $ \name -> do
  info <- reify name
  [| (name, info) |]

runMockQ :: MockQ -> Q a -> a
runMockQ mockQ = either error id . tryMockQ mockQ

runMockQErr :: Show a => MockQ -> Q a -> String
runMockQErr mockQ = either id (error . mkMsg) . tryMockQ mockQ
  where
    mkMsg a = "Unexpected success: " ++ show a

tryMockQ :: MockQ -> Q a -> Either String a
tryMockQ mockQ = runIdentity . runMockQMonad . runQ
  where
    runMockQMonad = runExceptT . (`evalStateT` Nothing) . (`runReaderT` mockQ) . unMockQMonad

newtype MockQMonad a = MockQMonad
  { unMockQMonad
      :: ReaderT MockQ
          ( StateT (Maybe String) -- the last message sent to qReport
              ( ExceptT String
                  Identity
              )
          )
          a
  }
  deriving (Functor, Applicative, Monad, MonadReader MockQ, MonadError String, MonadState (Maybe String))

instance MonadIO MockQMonad where
  liftIO io = do
    MockQ{runIOUnsafe} <- ask
    if runIOUnsafe
      then return $ unsafePerformIO io
      else error "IO actions not allowed"

instance MonadFail MockQMonad where
  fail msg = do
    -- The implementation of 'fail' for Q will send the message to qReport before calling 'fail'.
    -- Check to see if qReport put any message in the state and throw that message if so.
    lastMessage <- get
    throwError $ fromMaybe msg lastMessage

instance Quasi MockQMonad where
  qNewName name = return $ unsafePerformIO $ do
    Time.UTCTime day time <- Time.getCurrentTime
    let n = Time.toModifiedJulianDay day + Time.diffTimeToPicoseconds time
    return $ mkNameU name $ fromInteger n

  qLookupName _ name = do
    MockQ{knownNames} <- ask
    return $ lookup name knownNames

  qReify name = do
    MockQ{reifyInfo} <- ask
    case lookup name reifyInfo of
      Just info -> return info
      Nothing -> error $ "Cannot reify " ++ show name ++ " (did you mean to add it to reifyInfo?)"

  qRecover (MockQMonad handler) (MockQMonad action) = MockQMonad $ action `catchError` const handler
  qReport b msg = when b $ put (Just msg)

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
