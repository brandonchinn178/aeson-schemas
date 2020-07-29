{-|
Module      :  Data.Aeson.Schema.Internal
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Internal definitions for declaring JSON schemas.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Schema.Internal where

import Control.Applicative (Alternative(..))
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)
import Fcf (type (<=<), type (=<<))
import qualified Fcf
import GHC.Exts (toList)
import GHC.TypeLits
    (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

import Data.Aeson.Schema.Key
    ( IsSchemaKey(..)
    , SchemaKey
    , SchemaKey'(..)
    , SchemaKeyV
    , fromSchemaKeyV
    , getContext
    , showSchemaKey
    , toContext
    )
import Data.Aeson.Schema.Type
    ( FromSchema
    , Schema
    , Schema'(..)
    , SchemaType
    , SchemaType'(..)
    , SchemaTypeV
    , ToSchemaObject
    , showSchemaTypeV
    )
import Data.Aeson.Schema.Utils.All (All(..))
import Data.Aeson.Schema.Utils.Invariant (unreachable)
import Data.Aeson.Schema.Utils.Sum (SumType(..))

{- Schema-validated JSON object -}

-- | The object containing JSON data and its schema.
--
-- Has a 'FromJSON' instance, so you can use the usual 'Data.Aeson' decoding functions.
--
-- > obj = decode "{\"a\": 1}" :: Maybe (Object [schema| { a: Int } |])
newtype Object (schema :: Schema) = UnsafeObject (HashMap Text Dynamic)

instance IsSchemaType ('SchemaObject schema) => Show (Object ('Schema schema)) where
  show = showValue @('SchemaObject schema)

instance IsSchemaType ('SchemaObject schema) => Eq (Object ('Schema schema)) where
  a == b = toJSON a == toJSON b

instance IsSchemaType ('SchemaObject schema) => FromJSON (Object ('Schema schema)) where
  parseJSON = parseValue @('SchemaObject schema) []

instance IsSchemaType ('SchemaObject schema) => ToJSON (Object ('Schema schema)) where
  toJSON = toValue @('SchemaObject schema)

toMap :: IsSchema ('Schema schema) => Object ('Schema schema) -> Aeson.Object
toMap = toValueMap

{- Type-level schema definitions -}

type IsSchema (schema :: Schema) =
  ( IsSchemaType (ToSchemaObject schema)
  , All IsSchemaObjectPair (FromSchema schema)
  , FromJSON (Object schema)
  )

-- | Show the given schema.
--
-- Usage:
-- @
-- >>> type MySchema = [schema| { a: Int } |]
-- >>> showSchema @MySchema
-- @
showSchema :: forall (schema :: Schema). IsSchema schema => String
showSchema = showSchemaType @(ToSchemaObject schema)

showSchemaType :: forall (schemaType :: SchemaType). IsSchemaType schemaType => String
showSchemaType = showSchemaTypeV schemaType
  where
    schemaType = toSchemaTypeV $ Proxy @schemaType

{- Conversions from schema types into Haskell types -}

-- | A type family mapping SchemaType to the corresponding Haskell type.
type family SchemaResult (schema :: SchemaType) where
  SchemaResult ('SchemaScalar inner) = inner
  SchemaResult ('SchemaMaybe inner) = Maybe (SchemaResult inner)
  SchemaResult ('SchemaTry inner) = Maybe (SchemaResult inner)
  SchemaResult ('SchemaList inner) = [SchemaResult inner]
  SchemaResult ('SchemaUnion schemas) = SumType (SchemaResultList schemas)
  SchemaResult ('SchemaObject inner) = Object ('Schema inner)

type family SchemaResultList (xs :: [SchemaType]) where
  SchemaResultList '[] = '[]
  SchemaResultList (x ': xs) = SchemaResult x ': SchemaResultList xs

-- | A type-class for types that can be parsed from JSON for an associated schema type.
class IsSchemaType (schema :: SchemaType) where
  toSchemaTypeV :: Proxy schema -> SchemaTypeV

  parseValue :: [Text] -> Value -> Parser (SchemaResult schema)
  default parseValue :: FromJSON (SchemaResult schema) => [Text] -> Value -> Parser (SchemaResult schema)
  parseValue path value = parseJSON value <|> parseFail @schema path value

  toValue :: SchemaResult schema -> Value
  default toValue :: ToJSON (SchemaResult schema) => SchemaResult schema -> Value
  toValue = toJSON

  showValue :: SchemaResult schema -> String
  default showValue :: Show (SchemaResult schema) => SchemaResult schema -> String
  showValue = show

instance (Show inner, Typeable inner, FromJSON inner, ToJSON inner) => IsSchemaType ('SchemaScalar inner) where
  toSchemaTypeV _ = SchemaScalar (tyConName $ typeRepTyCon $ typeRep $ Proxy @inner)

instance (IsSchemaType inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => IsSchemaType ('SchemaMaybe inner) where
  toSchemaTypeV _ = SchemaMaybe (toSchemaTypeV $ Proxy @inner)

  parseValue path = \case
    Null -> return Nothing
    value -> (Just <$> parseValue @inner path value)

instance (IsSchemaType inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => IsSchemaType ('SchemaTry inner) where
  toSchemaTypeV _ = SchemaTry (toSchemaTypeV $ Proxy @inner)

  parseValue path = wrapTry . parseValue @inner path
    where
      wrapTry parser = (Just <$> parser) <|> pure Nothing

instance (IsSchemaType inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => IsSchemaType ('SchemaList inner) where
  toSchemaTypeV _ = SchemaList (toSchemaTypeV $ Proxy @inner)

  parseValue path = \case
    Array a -> traverse (parseValue @inner path) (toList a)
    value -> parseFail @('SchemaList inner) path value

instance
  ( All IsSchemaType schemas
  , Show (SchemaResult ('SchemaUnion schemas))
  , FromJSON (SchemaResult ('SchemaUnion schemas))
  , ToJSON (SchemaResult ('SchemaUnion schemas))
  , ParseSumType schemas
  ) => IsSchemaType ('SchemaUnion (schemas :: [SchemaType])) where
  toSchemaTypeV _ = SchemaUnion (mapAll @IsSchemaType @schemas toSchemaTypeV)

  parseValue path value = parseSumType @schemas path value <|> parseFail @('SchemaUnion schemas) path value

class ParseSumType xs where
  parseSumType :: [Text] -> Value -> Parser (SumType (SchemaResultList xs))

instance ParseSumType '[] where
  parseSumType _ _ = empty

instance (IsSchemaType schema, ParseSumType schemas) => ParseSumType (schema ': schemas) where
  parseSumType path value = parseHere <|> parseThere
    where
      parseHere = Here <$> parseValue @schema path value
      parseThere = There <$> parseSumType @schemas path value

instance All IsSchemaObjectPair pairs => IsSchemaType ('SchemaObject pairs) where
  toSchemaTypeV _ = SchemaObject (mapAll @IsSchemaObjectPair @pairs toSchemaTypeVPair)

  parseValue path = \case
    Aeson.Object o -> UnsafeObject . HashMap.fromList <$> parseValueMap o
    value -> parseFail @('SchemaObject pairs) path value
    where
      parseValueMap :: Aeson.Object -> Parser [(Text, Dynamic)]
      parseValueMap o = sequence $ mapAll @IsSchemaObjectPair @pairs $ \proxy -> parseValuePair proxy path o

  toValue = Aeson.Object . toValueMap

  showValue o = "{ " ++ intercalate ", " (map fromPair pairs) ++ " }"
    where
      fromPair (k, v) = k ++ ": " ++ v
      pairs = mapAll @IsSchemaObjectPair @pairs $ \proxy -> showValuePair proxy o

toValueMap :: forall pairs. All IsSchemaObjectPair pairs => Object ('Schema pairs) -> Aeson.Object
toValueMap o = HashMap.unions $ mapAll @IsSchemaObjectPair @pairs (\proxy -> toValuePair proxy o)

class IsSchemaObjectPair (a :: (SchemaKey, SchemaType)) where
  toSchemaTypeVPair :: Proxy a -> (SchemaKeyV, SchemaTypeV)
  parseValuePair :: Proxy a -> [Text] -> Aeson.Object -> Parser (Text, Dynamic)
  toValuePair :: Proxy a -> Object schema -> Aeson.Object
  showValuePair :: Proxy a -> Object schema -> (String, String)

instance
  ( IsSchemaKey key
  , IsSchemaType inner
  , Typeable (SchemaResult inner)
  ) => IsSchemaObjectPair '(key, inner) where
  toSchemaTypeVPair _ = (schemaKey, toSchemaTypeV $ Proxy @inner)
    where
      schemaKey = toSchemaKeyV $ Proxy @key

  parseValuePair _ path o = do
    inner <- parseValue @inner (key:path) $ getContext schemaKey o
    return (key, toDyn inner)
    where
      schemaKey = toSchemaKeyV $ Proxy @key
      key = Text.pack $ fromSchemaKeyV schemaKey

  toValuePair _ o = toContext schemaKey (toValue @inner val)
    where
      schemaKey = toSchemaKeyV $ Proxy @key
      val = unsafeGetKey @inner (Proxy @(FromSchemaKey key)) o

  showValuePair _ o = (showSchemaKey @key, showValue @inner val)
    where
      val = unsafeGetKey @inner (Proxy @(FromSchemaKey key)) o

-- | A helper for creating fail messages when parsing a schema.
parseFail :: forall (schema :: SchemaType) m a. (MonadFail m, IsSchemaType schema) => [Text] -> Value -> m a
parseFail path value = fail $ msg ++ ": " ++ ellipses 200 (show value)
  where
    msg = if null path
      then "Could not parse schema " ++ schema'
      else "Could not parse path '" ++ path' ++ "' with schema " ++ schema'
    path' = Text.unpack . Text.intercalate "." $ reverse path
    schema' = "`" ++ showSchemaType @schema ++ "`"
    ellipses n s = if length s > n then take n s ++ "..." else s

{- Lookups within SchemaObject -}

data UnSchemaKey :: SchemaKey -> Fcf.Exp Symbol
type instance Fcf.Eval (UnSchemaKey ('NormalKey key)) = Fcf.Eval (Fcf.Pure key)
type instance Fcf.Eval (UnSchemaKey ('PhantomKey key)) = Fcf.Eval (Fcf.Pure key)

-- first-class-families-0.3.0.1 doesn't support partially applying Lookup
type Lookup a = Fcf.Map Fcf.Snd <=< Fcf.Find (Fcf.TyEq a <=< Fcf.Fst)

-- | The type-level function that return the schema of the given key in a 'SchemaObject'.
type family LookupSchema (key :: Symbol) (schema :: Schema) :: SchemaType where
  LookupSchema key ('Schema schema) = Fcf.Eval
    ( Fcf.FromMaybe (TypeError
        (     'Text "Key '"
        ':<>: 'Text key
        ':<>: 'Text "' does not exist in the following schema:"
        ':$$: 'ShowType schema
        )
      )
      =<< Lookup key =<< Fcf.Map (Fcf.Bimap UnSchemaKey Fcf.Pure) schema
    )

-- | Get a key from the given 'Data.Aeson.Schema.Internal.Object', returned as the type encoded in
-- its schema.
--
-- > let o = .. :: Object
-- >             ( 'SchemaObject
-- >                '[ '("foo", 'SchemaInt)
-- >                 , '("bar", 'SchemaObject
-- >                      '[ '("name", 'SchemaText)
-- >                       ]
-- >                 , '("baz", 'SchemaMaybe 'SchemaBool)
-- >                 ]
-- >             )
-- >
-- > getKey (Proxy @"foo") o                  :: Bool
-- > getKey (Proxy @"bar") o                  :: Object ('SchemaObject '[ '("name", 'SchemaText) ])
-- > getKey (Proxy @"name") $ getKey @"bar" o :: Text
-- > getKey (Proxy @"baz") o                  :: Maybe Bool
--
getKey
  :: forall (key :: Symbol) (schema :: Schema) (endSchema :: SchemaType) result.
     ( endSchema ~ LookupSchema key schema
     , result ~ SchemaResult endSchema
     , KnownSymbol key
     , Typeable result
     , Typeable endSchema
     )
  => Proxy key
  -> Object schema
  -> result
getKey = unsafeGetKey @endSchema

unsafeGetKey
  :: forall (endSchema :: SchemaType) (key :: Symbol) (schema :: Schema)
  . (KnownSymbol key, Typeable (SchemaResult endSchema))
  => Proxy key -> Object schema -> SchemaResult endSchema
unsafeGetKey keyProxy (UnsafeObject object) =
  fromMaybe (unreachable $ "Could not load key: " ++ key) $
    fromDynamic (object ! Text.pack key)
  where
    key = symbolVal keyProxy
