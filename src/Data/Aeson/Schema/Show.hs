{-|
Module      :  Data.Aeson.Schema.Show
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Utilities for showing a schema. Meant to be imported qualified.
-}
{-# LANGUAGE LambdaCase #-}

module Data.Aeson.Schema.Show
  ( SchemaType(..)
  , showSchemaType
    -- * Re-exports
  , SchemaKey(..)
  ) where

import Data.List (intercalate)

import Data.Aeson.Schema.Key (SchemaKey(..), showSchemaKey)

-- | 'Data.Aeson.Schema.Internal.SchemaType', but for printing.
data SchemaType
  = SchemaScalar String
  | SchemaMaybe SchemaType
  | SchemaTry SchemaType
  | SchemaList SchemaType
  | SchemaObject [(SchemaKey, SchemaType)]
  | SchemaUnion [SchemaType]
  deriving (Show,Eq)

-- | Pretty show the given SchemaType.
showSchemaType :: SchemaType -> String
showSchemaType schema = case schema of
  SchemaScalar s -> "SchemaScalar " ++ s
  SchemaMaybe inner -> "SchemaMaybe " ++ showSchemaType' inner
  SchemaTry inner -> "SchemaTry " ++ showSchemaType' inner
  SchemaList inner -> "SchemaList " ++ showSchemaType' inner
  SchemaUnion _ -> "SchemaUnion " ++ showSchemaType' schema
  SchemaObject _ -> "SchemaObject " ++ showSchemaType' schema
  where
    showSchemaType' = \case
      SchemaScalar s -> s
      SchemaMaybe inner -> "Maybe " ++ showSchemaType' inner
      SchemaTry inner -> "Try " ++ showSchemaType' inner
      SchemaList inner -> "List " ++ showSchemaType' inner
      SchemaUnion schemas -> "( " ++ mapJoin showSchemaType' " | " schemas ++ " )"
      SchemaObject pairs -> "{" ++ mapJoin showPair ", " pairs ++ "}"
    showPair (key, inner) = showSchemaKey key ++ ": " ++ showSchemaType' inner

    mapJoin f delim = intercalate delim . map f
