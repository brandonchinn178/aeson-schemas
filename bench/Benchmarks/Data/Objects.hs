{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Benchmarks.Data.Objects where

import Data.Aeson (ToJSON (..), Value)
import Data.Dynamic (Dynamic, Typeable, toDyn)
import Data.Proxy (Proxy (..))
import Data.String (fromString)

import Data.Aeson.Schema.Internal (Object (..), SchemaResult)
import Data.Aeson.Schema.Key (IsSchemaKey, SchemaKey, fromSchemaKey)
import Data.Aeson.Schema.Type (
  IsSchemaObjectMap,
  SchemaType,
  SchemaType' (..),
  ToSchemaObject,
 )
import Data.Aeson.Schema.Utils.All (All (..))
import qualified Data.Aeson.Schema.Utils.Compat as Compat

type MockSchema schema =
  ( MockSchemaResult (ToSchemaObject schema)
  , Object schema ~ SchemaResult (ToSchemaObject schema)
  , ToJSON (Object schema)
  )

schemaObject :: forall schema. MockSchema schema => Object schema
schemaObject = schemaResult (Proxy @(ToSchemaObject schema))

schemaValue :: forall schema. MockSchema schema => Value
schemaValue = toJSON $ schemaObject @schema

class Typeable (SchemaResult schema) => MockSchemaResult (schema :: SchemaType) where
  schemaResult :: Proxy schema -> SchemaResult schema

instance MockSchemaResult ('SchemaScalar Int) where
  schemaResult _ = 42

instance
  ( All MockSchemaResultPair pairs
  , IsSchemaObjectMap pairs
  , Typeable pairs
  ) =>
  MockSchemaResult ('SchemaObject pairs)
  where
  schemaResult _ = UnsafeObject $ Compat.fromList $ mapAll @MockSchemaResultPair @pairs schemaResultPair

class MockSchemaResultPair (pair :: (SchemaKey, SchemaType)) where
  schemaResultPair :: Proxy pair -> (Compat.Key, Dynamic)

instance (IsSchemaKey key, MockSchemaResult inner) => MockSchemaResultPair '(key, inner) where
  schemaResultPair _ = (fromString $ fromSchemaKey @key, toDyn $ schemaResult $ Proxy @inner)
