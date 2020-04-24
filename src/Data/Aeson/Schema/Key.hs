{-|
Module      :  Data.Aeson.Schema.Key
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines a SchemaKey.
-}

module Data.Aeson.Schema.Key
  ( SchemaKey(..)
  , fromSchemaKey
  ) where

-- | A key in a JSON object schema.
data SchemaKey
  = NormalKey String
  deriving (Show)

fromSchemaKey :: SchemaKey -> String
fromSchemaKey (NormalKey key) = key
