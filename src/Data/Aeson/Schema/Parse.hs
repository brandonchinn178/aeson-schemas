{-|
Module      :  Data.Aeson.Schema.Parse
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Functions for parsing raw JSON data as schema-validated JSON objects.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aeson.Schema.Parse where

import Control.Monad ((>=>))
import Data.Aeson (Value, eitherDecode)
import Data.ByteString.Lazy (ByteString)

import Data.Aeson.Schema.Internal (FromSchema(..), SchemaType)

-- | Decode the given JSON string with the given schema.
--
-- Works best with the @TypeApplications@ language extension:
--
-- > decodeWithSchema @MySchema input
decodeWithSchema :: forall (schema :: SchemaType) result.
  ( FromSchema schema
  , SchemaResult schema ~ result
  ) => ByteString -> Either String result
decodeWithSchema = eitherDecode >=> parseSchema @schema

-- | Parse the given JSON data with the given schema.
--
-- Works best with the @TypeApplications@ language extension:
--
-- > parseSchema @MySchema value
parseSchema :: forall (schema :: SchemaType) result.
  ( FromSchema schema
  , SchemaResult schema ~ result
  ) => Value -> Either String result
parseSchema = parseValue @schema []
