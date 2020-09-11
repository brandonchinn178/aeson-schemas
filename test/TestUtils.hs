{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module TestUtils
  ( ShowSchemaResult(..)
  , json
  , parseValue
  , parseObject
  , parseProxy
  , mkExpQQ
  , testGolden
  , testParseError
  ) where

import Data.Aeson (FromJSON(..), Value, eitherDecode)
import Data.Aeson.Types (parseEither)
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Data.Typeable (Typeable, typeRep)
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

import Data.Aeson.Schema (IsSchema, Object, schema)
import qualified Data.Aeson.Schema.Internal as Internal

{- ShowSchemaResult -}

class ShowSchemaResult a where
  showSchemaResult :: String

instance IsSchema schema => ShowSchemaResult (Object schema) where
  showSchemaResult = "Object (" ++ Internal.showSchema @schema ++ ")"

instance ShowSchemaResult a => ShowSchemaResult [a] where
  showSchemaResult = "[" ++ showSchemaResult @a ++ "]"

instance {-# OVERLAPPABLE #-} Typeable a => ShowSchemaResult a where
  showSchemaResult = show $ typeRep (Proxy @a)

{- Loading JSON data -}

json :: QuasiQuoter
json = mkExpQQ $ \s -> [| (either error id . eitherDecode . fromString) s |]

parseValue :: FromJSON a => Value -> a
parseValue = either error id . parseEither parseJSON

parseProxy :: FromJSON a => Proxy a -> Value -> Either String a
parseProxy _ = parseEither parseJSON

parseObject :: String -> ExpQ
parseObject schemaString = [| parseValue :: Value -> Object $schemaType |]
  where
    schemaType = quoteType schema schemaString

{- QuasiQuotation -}

mkExpQQ :: (String -> ExpQ) -> QuasiQuoter
mkExpQQ f = QuasiQuoter
  { quoteExp = f
  , quotePat = error "Cannot use this QuasiQuoter for patterns"
  , quoteType = error "Cannot use this QuasiQuoter for types"
  , quoteDec = error "Cannot use this QuasiQuoter for declarations"
  }

{- Tasty test trees -}

testGolden :: String -> FilePath -> String -> TestTree
testGolden name fp s = goldenVsString name ("test/goldens/" ++ fp) $ return $ fromString s

-- | A golden test for testing parse errors.
testParseError :: String -> FilePath -> String -> TestTree
testParseError = testGolden
