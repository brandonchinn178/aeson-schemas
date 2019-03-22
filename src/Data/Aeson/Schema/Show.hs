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
  ) where

import Data.List (intercalate)

-- | 'Data.Aeson.Schema.Internal.SchemaType', but for printing.
data SchemaType
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaCustom String
  | SchemaMaybe SchemaType
  | SchemaList SchemaType
  | SchemaObject [(String, SchemaType)]
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
  SchemaList inner -> "SchemaList " ++ showSchemaType' inner
  SchemaObject _ -> "SchemaObject " ++ showSchemaType' schema
  where
    showSchemaType' = \case
      SchemaBool -> "Bool"
      SchemaInt -> "Int"
      SchemaDouble -> "Double"
      SchemaText -> "Text"
      SchemaCustom s -> s
      SchemaMaybe inner -> "Maybe " ++ showSchemaType' inner
      SchemaList inner -> "List " ++ showSchemaType' inner
      SchemaObject pairs -> "{" ++ intercalate ", " (map showPair pairs) ++ "}"
    showPair (key, inner) = "\"" ++ key ++ "\": " ++ showSchemaType' inner
