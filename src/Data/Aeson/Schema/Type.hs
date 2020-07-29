{-|
Module      :  Data.Aeson.Schema.Type
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines SchemaType, the AST that defines a JSON schema.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Data.Aeson.Schema.Type
  ( Schema'(..)
  , SchemaType'(..)
  , SchemaV
  , SchemaTypeV
  , showSchemaTypeV
  , Schema
  , SchemaType
  , ToSchemaObject
  , FromSchema
  , IsSchemaType(..)
  , IsSchemaObjectMap
  ) where

import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)
import GHC.TypeLits (Symbol)
import Language.Haskell.TH.Syntax (Lift)

import Data.Aeson.Schema.Key
    (IsSchemaKey(..), SchemaKey, SchemaKey', SchemaKeyV, showSchemaKeyV)
import Data.Aeson.Schema.Utils.All (All(..))

-- | The schema definition for a JSON object.
data Schema' s ty = Schema (SchemaObjectMap' s ty)

-- | The AST defining a JSON schema.
data SchemaType' s ty
  = SchemaScalar ty
  | SchemaMaybe (SchemaType' s ty)
  | SchemaTry (SchemaType' s ty) -- ^ @since v1.2.0
  | SchemaList (SchemaType' s ty)
  | SchemaUnion [SchemaType' s ty] -- ^ @since v1.1.0
  | SchemaObject (SchemaObjectMap' s ty)
  deriving (Show, Eq, Lift)

type SchemaObjectMap' s ty = [(SchemaKey' s, SchemaType' s ty)]

-- | Value-level schema types.
type SchemaV = Schema' String String
type SchemaTypeV = SchemaType' String String

-- | Pretty show the given SchemaType.
showSchemaTypeV :: SchemaTypeV -> String
showSchemaTypeV schema = case schema of
  SchemaScalar s -> "SchemaScalar " ++ s
  SchemaMaybe inner -> "SchemaMaybe " ++ showSchemaTypeV' inner
  SchemaTry inner -> "SchemaTry " ++ showSchemaTypeV' inner
  SchemaList inner -> "SchemaList " ++ showSchemaTypeV' inner
  SchemaUnion _ -> "SchemaUnion " ++ showSchemaTypeV' schema
  SchemaObject _ -> "SchemaObject " ++ showSchemaTypeV' schema
  where
    showSchemaTypeV' = \case
      SchemaScalar s -> s
      SchemaMaybe inner -> "Maybe " ++ showSchemaTypeV' inner
      SchemaTry inner -> "Try " ++ showSchemaTypeV' inner
      SchemaList inner -> "List " ++ showSchemaTypeV' inner
      SchemaUnion schemas -> "( " ++ mapJoin showSchemaTypeV' " | " schemas ++ " )"
      SchemaObject pairs -> "{ " ++ mapJoin showPair ", " pairs ++ " }"
    showPair (key, inner) = showSchemaKeyV key ++ ": " ++ showSchemaTypeV' inner

    mapJoin f delim = intercalate delim . map f

-- | Type-level schema types.
type Schema = Schema' Symbol Type
type SchemaType = SchemaType' Symbol Type
type SchemaObjectMap = SchemaObjectMap' Symbol Type

type family ToSchemaObject (schema :: Schema) :: SchemaType where
  ToSchemaObject ('Schema schema) = 'SchemaObject schema

type family FromSchema (schema :: Schema) :: SchemaObjectMap where
  FromSchema ('Schema schema) = schema

class IsSchemaType (schemaType :: SchemaType) where
  toSchemaTypeV :: Proxy schemaType -> SchemaTypeV

instance Typeable inner => IsSchemaType ('SchemaScalar inner) where
  toSchemaTypeV _ = SchemaScalar (tyConName $ typeRepTyCon $ typeRep $ Proxy @inner)

instance IsSchemaType inner => IsSchemaType ('SchemaMaybe inner) where
  toSchemaTypeV _ = SchemaMaybe (toSchemaTypeV $ Proxy @inner)

instance IsSchemaType inner => IsSchemaType ('SchemaTry inner) where
  toSchemaTypeV _ = SchemaTry (toSchemaTypeV $ Proxy @inner)

instance IsSchemaType inner => IsSchemaType ('SchemaList inner) where
  toSchemaTypeV _ = SchemaList (toSchemaTypeV $ Proxy @inner)

instance All IsSchemaType schemas => IsSchemaType ('SchemaUnion schemas) where
  toSchemaTypeV _ = SchemaUnion (mapAll @IsSchemaType @schemas toSchemaTypeV)

instance IsSchemaObjectMap pairs => IsSchemaType ('SchemaObject pairs) where
  toSchemaTypeV _ = SchemaObject (mapAll @IsSchemaObjectPair @pairs toSchemaTypePairV)

type IsSchemaObjectMap (pairs :: SchemaObjectMap) = All IsSchemaObjectPair pairs

class IsSchemaObjectPair (a :: (SchemaKey, SchemaType)) where
  toSchemaTypePairV :: Proxy a -> (SchemaKeyV, SchemaTypeV)

instance (IsSchemaKey key, IsSchemaType inner) => IsSchemaObjectPair '(key, inner) where
  toSchemaTypePairV _ = (toSchemaKeyV $ Proxy @key, toSchemaTypeV $ Proxy @inner)
