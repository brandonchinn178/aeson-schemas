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

import Data.Aeson.Schema.Key (SchemaKey(..), fromSchemaKey)

-- | 'Data.Aeson.Schema.Internal.SchemaType', but for printing.
data SchemaType
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaCustom String
  | SchemaMaybe SchemaType
  | SchemaTry SchemaType
  | SchemaList SchemaType
  | SchemaObject [(SchemaKey, SchemaType)]
  | SchemaUnion [SchemaType]
  deriving (Show)

-- | Pretty show the given SchemaType.
showSchemaType :: SchemaType -> String
showSchemaType schema = case schema of
  SchemaBool -> "SchemaBool"
  SchemaInt -> "SchemaInt"
  SchemaDouble -> "SchemaDouble"
  SchemaText -> "SchemaText"
  SchemaCustom s -> "SchemaCustom " ++ s
  SchemaMaybe inner -> "SchemaMaybe " ++ showSchemaType' inner
  SchemaTry inner -> "SchemaTry " ++ showSchemaType' inner
  SchemaList inner -> "SchemaList " ++ showSchemaType' inner
  SchemaObject _ -> "SchemaObject " ++ showSchemaType' schema
  SchemaUnion _ -> "SchemaUnion " ++ showSchemaType' schema
  where
    showSchemaType' = \case
      SchemaBool -> "Bool"
      SchemaInt -> "Int"
      SchemaDouble -> "Double"
      SchemaText -> "Text"
      SchemaCustom s -> s
      SchemaMaybe inner -> "Maybe " ++ showSchemaType' inner
      SchemaTry inner -> "Try " ++ showSchemaType' inner
      SchemaList inner -> "List " ++ showSchemaType' inner
      SchemaObject pairs -> "{" ++ mapJoin showPair ", " pairs ++ "}"
      SchemaUnion schemas -> "( " ++ mapJoin showSchemaType' " | " schemas ++ " )"
    showPair (key, inner) = "\"" ++ fromSchemaKey key ++ "\": " ++ showSchemaType' inner

    mapJoin f delim = intercalate delim . map f
