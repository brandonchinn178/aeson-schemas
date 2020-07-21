{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module TestUtils.TestQ
  ( -- * Configuring TestQ
    QState(..)
  , MockedMode(..)
  , QMode(..)
  , ReifyInfo(..)
  , loadNames
    -- * Running TestQ
  , runTestQ
  , runTestQErr
  , tryTestQ
  ) where

import Control.Monad (when)
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import Data.Maybe (fromMaybe)
import qualified Data.Time as Time
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax (Lift, Quasi(..), mkNameU)
#if MIN_VERSION_template_haskell(2,16,0)
import qualified Language.Haskell.TH.Syntax as TH
#endif
import System.IO.Unsafe (unsafePerformIO)

data MockedMode = FullyMocked | MockQLiveIO | NotMocked

class IsMockedMode (mode :: MockedMode) where
  type TestQResult mode a

  runResult :: Q a -> TestQResult mode a
  fmapResult :: (a -> b) -> TestQResult mode a -> TestQResult mode b

instance IsMockedMode 'FullyMocked where
  type TestQResult 'FullyMocked a = a
  runResult = unsafePerformIO . runQ
  fmapResult = ($)

instance IsMockedMode 'MockQLiveIO where
  type TestQResult 'MockQLiveIO a = IO a
  runResult = runQ
  fmapResult = fmap

instance IsMockedMode 'NotMocked where
  type TestQResult 'NotMocked a = Q a
  runResult = id
  fmapResult = fmap

{- Configuring TestQ -}

data QMode (mode :: MockedMode) where
  -- | All Q actions are mocked and IO actions are disallowed.
  MockQ        :: QMode 'FullyMocked

  -- | Same as MockQ, except IO actions are passed through.
  --
  -- Useful if your TH code, for example, reads files with runIO.
  MockQAllowIO :: QMode 'MockQLiveIO

  -- | No mocking is done.
  --
  -- Useful for running Q as normal, but you need to get error messages.
  AllowQ       :: QMode 'NotMocked

deriving instance Show (QMode mode)
deriving instance Lift (QMode mode)

-- | State information for mocking Q functionality.
data QState (mode :: MockedMode) = QState
  { mode       :: QMode mode
  , knownNames :: [(String, Name)]
    -- ^ Names that can be looked up with `lookupName`/`lookupTypeName`/`lookupValueName`
  , reifyInfo  :: [(Name, ReifyInfo)]
    -- ^ Reification information for Names to return when 'reify' is called.
  } deriving (Show, Lift)

data ReifyInfo = ReifyInfo
  { reifyInfoInfo   :: Info
  , reifyInfoFixity :: Maybe Fixity
  , reifyInfoRoles  :: [Role]
  , reifyInfoType   :: Type
  } deriving (Show, Lift)

-- | A helper for loading names for 'reifyInfo'
--
-- Usage:
--   QState
--     { reifyInfo = $(loadNames [''Int, ''Maybe])
--     , ...
--     }
loadNames :: [Name] -> ExpQ
loadNames names = listE $ flip map names $ \name -> do
  info <- reify name
  fixity <- reifyFixity name
  roles <- reifyRoles name
#if MIN_VERSION_template_haskell(2,16,0)
  infoType <- reifyType name >>= TH.lift
#else
  let infoType = [| error "Your version of template-haskell does not have 'reifyType'" |]
#endif

  [| (name, ReifyInfo info fixity roles $infoType) |]

{- TestQ entrypoints -}

runTestQ :: forall mode a. IsMockedMode mode => QState mode -> Q a -> TestQResult mode a
runTestQ state = fmapResult' (either error id) . tryTestQ state
  where
    fmapResult' = fmapResult @mode @(Either String a) @a

runTestQErr :: forall mode a. (IsMockedMode mode, Show a) => QState mode -> Q a -> TestQResult mode String
runTestQErr state = fmapResult' (either id (error . mkMsg)) . tryTestQ state
  where
    fmapResult' = fmapResult @mode @(Either String a) @String
    mkMsg a = "Unexpected success: " ++ show a

tryTestQ :: forall mode a. IsMockedMode mode => QState mode -> Q a -> TestQResult mode (Either String a)
tryTestQ state = runResult @mode . runTestQMonad . runQ
  where
    runTestQMonad =
      Except.runExceptT
      . (`State.evalStateT` Nothing)
      . (`Reader.runReaderT` state)
      . unTestQ

{- TestQ monad -}

newtype TestQ (mode :: MockedMode) a = TestQ
  { unTestQ
      :: Reader.ReaderT (QState mode)
          ( State.StateT (Maybe String) -- the last message sent to qReport
              ( Except.ExceptT String
                  Q
              )
          )
          a
  } deriving (Functor, Applicative, Monad)

getState :: TestQ mode (QState mode)
getState = TestQ Reader.ask

getMode :: TestQ mode (QMode mode)
getMode = mode <$> getState

lookupReifyInfo :: (ReifyInfo -> a) -> Name -> TestQ mode a
lookupReifyInfo f name = do
  QState{reifyInfo} <- getState
  case lookup name reifyInfo of
    Just info -> return $ f info
    Nothing -> error $ "Cannot reify " ++ show name ++ " (did you mean to add it to reifyInfo?)"

getLastError :: TestQ mode (Maybe String)
getLastError = TestQ (lift State.get)

storeLastError :: String -> TestQ mode ()
storeLastError = TestQ . lift . State.put . Just

throwError :: String -> TestQ mode a
throwError = TestQ . lift . lift . Except.throwE

catchError :: TestQ mode a -> (String -> TestQ mode a) -> TestQ mode a
catchError (TestQ action) handler = TestQ $ catchE' action (unTestQ . handler)
  where
    catchE' = Reader.liftCatch (State.liftCatch Except.catchE)

liftQ :: Q a -> TestQ mode a
liftQ = TestQ . lift . lift . lift

instance MonadIO (TestQ mode) where
  liftIO = liftQ . runIO

instance MonadFail (TestQ mode) where
  fail msg = do
    -- The implementation of 'fail' for Q will send the message to qReport before calling 'fail'.
    -- Check to see if qReport put any message in the state and throw that message if so.
    lastMessage <- getLastError
    throwError $ fromMaybe msg lastMessage

guardMocked :: Q a -> TestQ mode a -> TestQ mode a
guardMocked q testQ = getMode >>= \case
  AllowQ -> liftQ q
  _ -> testQ

unsupported :: String -> Q a -> TestQ mode a
unsupported label q = guardMocked q $ error $ "Cannot run '" ++ label ++ "' with TestQ"

instance Quasi (TestQ mode) where
  {- Error handling + reporting -}

  qRecover handler action = action `catchError` const handler

  qReport b msg = do
    when b $ storeLastError msg
    getMode >>= \case
      AllowQ -> liftQ $ qReport b msg
      _ -> return ()

  {- Names -}

  qNewName name = guardMocked (qNewName name) $ liftIO $ do
    Time.UTCTime day time <- Time.getCurrentTime
    let n = Time.toModifiedJulianDay day + Time.diffTimeToPicoseconds time
    return $ mkNameU name $ fromInteger n

  qLookupName b name = guardMocked (qLookupName b name) $ do
    QState{knownNames} <- getState
    return $ lookup name knownNames

  {- ReifyInfo -}

  qReify name = guardMocked (qReify name) $
    lookupReifyInfo reifyInfoInfo name

  qReifyFixity name = guardMocked (qReifyFixity name) $
    lookupReifyInfo reifyInfoFixity name

  qReifyRoles name = guardMocked (qReifyRoles name) $
    lookupReifyInfo reifyInfoRoles name

#if MIN_VERSION_template_haskell(2,16,0)
  qReifyType name = guardMocked (qReifyType name) $
    lookupReifyInfo reifyInfoType name
#endif

  {- IO -}

  qRunIO io = getMode >>= \case
    MockQ -> error "IO actions not allowed"
    _ -> liftIO io

  {- Currently unsupported -}

  qReifyInstances name types = unsupported "qReifyInstances" (qReifyInstances name types)
  qReifyAnnotations annlookup = unsupported "qReifyAnnotations" (qReifyAnnotations annlookup)
  qReifyModule mod' = unsupported "qReifyModule" (qReifyModule mod')
  qReifyConStrictness name = unsupported "qReifyConStrictness" (qReifyConStrictness name)
  qLocation = unsupported "qLocation" qLocation
  qAddDependentFile fp = unsupported "qAddDependentFile" (qAddDependentFile fp)
  qAddTopDecls decls = unsupported "qAddTopDecls" (qAddTopDecls decls)
  qAddModFinalizer q = unsupported "qAddModFinalizer" (qAddModFinalizer q)
  qGetQ = unsupported "qGetQ" qGetQ
  qPutQ a = unsupported "qPutQ" (qPutQ a)
  qIsExtEnabled ext = unsupported "qIsExtEnabled" (qIsExtEnabled ext)
  qExtsEnabled = unsupported "qExtsEnabled" qExtsEnabled

#if MIN_VERSION_template_haskell(2,13,0)
  qAddCorePlugin plugin = unsupported "qAddCorePlugin" (qAddCorePlugin plugin)
#endif

#if MIN_VERSION_template_haskell(2,14,0)
  qAddTempFile suffix = unsupported "qAddTempFile" (qAddTempFile suffix)
  qAddForeignFilePath lang fp = unsupported "qAddForeignFilePath" (qAddForeignFilePath lang fp)
#elif MIN_VERSION_template_haskell(2,12,0)
  qAddForeignFile lang fp = unsupported "qAddForeignFile" (qAddForeignFile lang fp)
#endif
