{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TestUtils (
  ShowSchemaResult (..),
  json,
  parseValue,
  parseObject,
  parseProxy,
  mkExpQQ,
  testGolden,
  testGoldenIO,
  testParseError,
  ghcGoldenDir,
  ghcVersion,
) where

import Data.Aeson (FromJSON (..), Value, eitherDecode)
import Data.Aeson.Types (parseEither)

import Data.Proxy (Proxy (..))
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import Data.Typeable (Typeable, typeRep)
import Data.Version (Version, makeVersion, showVersion, versionBranch)
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Golden.Advanced (goldenTest)

import Data.Aeson.Schema (IsSchema, Object, schema)
import qualified Data.Aeson.Schema.Internal as Internal

{- ShowSchemaResult -}

class ShowSchemaResult a where
  showSchemaResult :: String

instance (IsSchema schema) => ShowSchemaResult (Object schema) where
  showSchemaResult = "Object (" ++ Internal.showSchema @schema ++ ")"

instance (ShowSchemaResult a) => ShowSchemaResult [a] where
  showSchemaResult = "[" ++ showSchemaResult @a ++ "]"

instance {-# OVERLAPPABLE #-} (Typeable a) => ShowSchemaResult a where
  showSchemaResult = show $ typeRep (Proxy @a)

{- Loading JSON data -}

json :: QuasiQuoter
json = mkExpQQ $ \s -> [|(either error id . eitherDecode . fromString) s|]

parseValue :: (FromJSON a) => Value -> a
parseValue = either error id . parseEither parseJSON

parseProxy :: (FromJSON a) => Proxy a -> Value -> Either String a
parseProxy _ = parseEither parseJSON

parseObject :: String -> ExpQ
parseObject schemaString = [|parseValue :: Value -> Object $schemaType|]
  where
    schemaType = quoteType schema schemaString

{- QuasiQuotation -}

mkExpQQ :: (String -> ExpQ) -> QuasiQuoter
mkExpQQ f =
  QuasiQuoter
    { quoteExp = f
    , quotePat = error "Cannot use this QuasiQuoter for patterns"
    , quoteType = error "Cannot use this QuasiQuoter for types"
    , quoteDec = error "Cannot use this QuasiQuoter for declarations"
    }

{- Tasty test trees -}

testGolden :: String -> FilePath -> String -> TestTree
testGolden name fp = testGoldenIO name fp . return

testGoldenIO :: String -> FilePath -> IO String -> TestTree
testGoldenIO name fp = goldenVsString name ("test/goldens/" ++ fp) . fmap toByteString
  where
    toByteString = TextL.encodeUtf8 . TextL.fromStrict . Text.pack

-- | A golden test for testing parse errors.
testParseError :: String -> FilePath -> String -> TestTree
testParseError name fp s = goldenTest name getExpected getActual cmp update
  where
    goldenFile = "test/goldens/" ++ fp
    getExpected = Text.readFile goldenFile
    getActual = return $ Text.pack s
    cmp expected actual =
      return $
        if expected == actual
          then Nothing
          else Just $ "Test output was different from '" ++ goldenFile ++ "'. It was:\n" ++ Text.unpack actual
    update = Text.writeFile goldenFile

-- | The directory to put GHC version-specific golden files.
ghcGoldenDir :: FilePath
ghcGoldenDir = "ghc" </> showVersion ghcMinorVersion
  where
    ghcMinorVersion = makeVersion . take 2 . versionBranch $ ghcVersion

ghcVersion :: Version
ghcVersion = makeVersion . map (read . Text.unpack) $ Text.splitOn "." __GLASGOW_HASKELL_FULL_VERSION__
