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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Schema.Internal where

import Control.Applicative ((<|>))
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Data.Aeson (FromJSON(..), Value(..))
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import Data.Dynamic (Dynamic, fromDyn, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, splitTyConApp, tyConName, typeRep, typeRepTyCon)
import Fcf (type (=<<), Eval, FromMaybe, Lookup)
import GHC.Exts (toList)
import GHC.TypeLits
    (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

import qualified Data.Aeson.Schema.Show as SchemaShow
import Data.Aeson.Schema.Utils.Sum (SumType)
import Data.Aeson.Schema.Utils.TypeFamilies (All)

{- Schema-validated JSON object -}

-- | The object containing JSON data and its schema.
--
-- Has a 'FromJSON' instance, so you can use the usual 'Data.Aeson' decoding functions.
--
-- > obj = decode "{\"a\": 1}" :: Maybe (Object [schema| { a: Int } |])
newtype Object (schema :: SchemaType) = UnsafeObject (HashMap Text Dynamic)

-- | A constraint that checks if the given schema is a 'SchemaObject.
type IsSchemaObject schema = (IsSchemaType schema, SchemaResult schema ~ Object schema)

instance IsSchemaObject schema => Show (Object schema) where
  show = showValue @schema

instance IsSchemaObject schema => FromJSON (Object schema) where
  parseJSON = parseValue @schema []

{- Type-level schema definitions -}

-- | The type-level schema definition for JSON data.
--
-- To view a schema for debugging, use 'showSchema'.
data SchemaType
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaCustom Type
  | SchemaMaybe SchemaType
  | SchemaList SchemaType
  | SchemaObject [(Symbol, SchemaType)]
  | SchemaUnion [SchemaType] -- ^ @since v1.1.0

-- | Convert 'SchemaType' into 'SchemaShow.SchemaType'.
toSchemaTypeShow :: forall (a :: SchemaType). Typeable a => SchemaShow.SchemaType
toSchemaTypeShow = cast $ typeRep (Proxy @a)
  where
    cast tyRep = case splitTypeRep tyRep of
      ("'SchemaBool", _) -> SchemaShow.SchemaBool
      ("'SchemaInt", _) -> SchemaShow.SchemaInt
      ("'SchemaDouble", _) -> SchemaShow.SchemaDouble
      ("'SchemaText", _) -> SchemaShow.SchemaText
      ("'SchemaCustom", [inner]) -> SchemaShow.SchemaCustom $ typeRepName inner
      ("'SchemaMaybe", [inner]) -> SchemaShow.SchemaMaybe $ cast inner
      ("'SchemaList", [inner]) -> SchemaShow.SchemaList $ cast inner
      ("'SchemaObject", [pairs]) -> SchemaShow.SchemaObject $ map getSchemaObjectPair $ typeRepToList pairs
      ("'SchemaUnion", [schemas]) -> SchemaShow.SchemaUnion $ map cast $ typeRepToList schemas
      _ -> error $ "Unknown schema type: " ++ show tyRep

    getSchemaObjectPair tyRep =
      let (key, val) = typeRepToPair tyRep
          key' = tail . init . typeRepName $ key -- strip leading + trailing quote
      in (key', cast val)

    typeRepToPair tyRep = case splitTypeRep tyRep of
      ("'(,)", [a, b]) -> (a, b)
      _ -> error $ "Unknown pair: " ++ show tyRep

    typeRepToList tyRep = case splitTypeRep tyRep of
      ("'[]", []) -> []
      ("':", [x, rest]) -> x : typeRepToList rest
      _ -> error $ "Unknown list: " ++ show tyRep

    splitTypeRep = first tyConName . splitTyConApp
    typeRepName = tyConName . typeRepTyCon

-- | Pretty show the given SchemaType.
showSchema :: forall (a :: SchemaType). Typeable a => String
showSchema = SchemaShow.showSchemaType $ toSchemaTypeShow @a

{- Conversions from schema types into Haskell types -}

-- | A type family mapping SchemaType to the corresponding Haskell type.
type family SchemaResult (schema :: SchemaType) where
  SchemaResult 'SchemaBool = Bool
  SchemaResult 'SchemaInt = Int
  SchemaResult 'SchemaDouble = Double
  SchemaResult 'SchemaText = Text
  SchemaResult ('SchemaCustom inner) = inner
  SchemaResult ('SchemaMaybe inner) = Maybe (SchemaResult inner)
  SchemaResult ('SchemaList inner) = [SchemaResult inner]
  SchemaResult ('SchemaObject inner) = Object ('SchemaObject inner)
  SchemaResult ('SchemaUnion schemas) = SumType (SchemaResultList schemas)

type family SchemaResultList (xs :: [SchemaType]) where
  SchemaResultList '[] = '[]
  SchemaResultList (x ': xs) = SchemaResult x ': SchemaResultList xs

-- | A type-class for types that can be parsed from JSON for an associated schema type.
class Typeable schema => IsSchemaType (schema :: SchemaType) where
  parseValue :: [Text] -> Value -> Parser (SchemaResult schema)
  default parseValue :: FromJSON (SchemaResult schema) => [Text] -> Value -> Parser (SchemaResult schema)
  parseValue path value = parseJSON value <|> parseFail @schema path value

  showValue :: SchemaResult schema -> String
  default showValue :: Show (SchemaResult schema) => SchemaResult schema -> String
  showValue = show

instance IsSchemaType 'SchemaBool

instance IsSchemaType 'SchemaInt

instance IsSchemaType 'SchemaDouble

instance IsSchemaType 'SchemaText

instance (Show inner, Typeable inner, FromJSON inner) => IsSchemaType ('SchemaCustom inner)

instance (IsSchemaType inner, Show (SchemaResult inner)) => IsSchemaType ('SchemaMaybe inner) where
  parseValue path = \case
    Null -> return Nothing
    value -> (Just <$> parseValue @inner path value)

instance (IsSchemaType inner, Show (SchemaResult inner)) => IsSchemaType ('SchemaList inner) where
  parseValue path value = case value of
    Array a -> traverse (parseValue @inner path) (toList a)
    _ -> parseFail @('SchemaList inner) path value

instance IsSchemaType ('SchemaObject '[]) where
  parseValue path = \case
    Object _ -> return $ UnsafeObject mempty
    value -> parseFail @('SchemaObject '[]) path value

  showValue _ = "{}"

instance
  ( KnownSymbol key
  , IsSchemaType inner
  , Show (SchemaResult inner)
  , Typeable (SchemaResult inner)
  , IsSchemaObject ('SchemaObject rest)
  , Typeable rest
  ) => IsSchemaType ('SchemaObject ('(key, inner) ': rest)) where
  parseValue path value = case value of
    Object o -> do
      let key = Text.pack $ symbolVal $ Proxy @key
          innerVal = fromMaybe Null $ HashMap.lookup key o

      inner <- parseValue @inner (key:path) innerVal
      UnsafeObject rest <- parseValue @('SchemaObject rest) path value

      return $ UnsafeObject $ HashMap.insert key (toDyn inner) rest
    _ -> parseFail @('SchemaObject ('(key, inner) ': rest)) path value

  showValue (UnsafeObject hm) = case showValue @('SchemaObject rest) (UnsafeObject hm) of
    "{}" -> "{" ++ pair ++ "}"
    '{':s -> "{" ++ pair ++ ", " ++ s
    s -> error $ "Unknown result when showing Object: " ++ s
    where
      key = symbolVal $ Proxy @key
      value =
        let dynValue = hm ! Text.pack key
        in maybe (show dynValue) show $ fromDynamic @(SchemaResult inner) dynValue
      pair = "\"" ++ key ++ "\": " ++ value

instance
  ( All IsSchemaType schemas
  , Typeable schemas
  , Show (SchemaResult ('SchemaUnion schemas))
  , FromJSON (SchemaResult ('SchemaUnion schemas))
  ) => IsSchemaType ('SchemaUnion schemas)

-- | A helper for creating fail messages when parsing a schema.
parseFail :: forall (schema :: SchemaType) m a. (MonadFail m, Typeable schema) => [Text] -> Value -> m a
parseFail path value = fail $ msg ++ ": " ++ ellipses 200 (show value)
  where
    msg = if null path
      then "Could not parse schema " ++ schema'
      else "Could not parse path '" ++ path' ++ "' with schema " ++ schema'
    path' = Text.unpack . Text.intercalate "." $ reverse path
    schema' = "`" ++ showSchema @schema ++ "`"
    ellipses n s = if length s > n then take n s ++ "..." else s

{- Lookups within SchemaObject -}

-- | The type-level function that return the schema of the given key in a 'SchemaObject'.
type family LookupSchema (key :: Symbol) (schema :: SchemaType) :: SchemaType where
  LookupSchema key ('SchemaObject schema) = Eval
    ( FromMaybe (TypeError
        (     'Text "Key '"
        ':<>: 'Text key
        ':<>: 'Text "' does not exist in the following schema:"
        ':$$: 'ShowType schema
        )
      )
      =<< Lookup key schema
    )
  LookupSchema key schema = TypeError
    (     'Text "Attempted to lookup key '"
    ':<>: 'Text key
    ':<>: 'Text "' in the following schema:"
    ':$$: 'ShowType schema
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
-- > getKey @"foo" o                  :: Bool
-- > getKey @"bar" o                  :: Object ('SchemaObject '[ '("name", 'SchemaText) ])
-- > getKey @"name" $ getKey @"bar" o :: Text
-- > getKey @"baz" o                  :: Maybe Bool
--
getKey
  :: forall key schema endSchema result
   . ( endSchema ~ LookupSchema key schema
     , result ~ SchemaResult endSchema
     , KnownSymbol key
     , Typeable result
     , Typeable endSchema
     )
  => Object schema
  -> result
getKey (UnsafeObject object) = fromDyn (object ! Text.pack key) badCast
  where
    key = symbolVal (Proxy @key)
    badCast = error $ "Could not load key: " ++ key
