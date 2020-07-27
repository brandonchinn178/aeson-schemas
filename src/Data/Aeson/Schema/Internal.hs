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

import Control.Applicative ((<|>))
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Type)
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

import qualified Data.Aeson.Schema.Show as SchemaShow
import Data.Aeson.Schema.Utils.All (All(..))
import Data.Aeson.Schema.Utils.Sum (SumType)

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

type SchemaObjectMap = [(SchemaKey, SchemaType)]

-- | The type-level schema definition for JSON data.
--
-- To view a schema for debugging, use 'showSchema'.
data Schema = Schema SchemaObjectMap

type family ToSchemaObject (schema :: Schema) where
  ToSchemaObject ('Schema schema) = 'SchemaObject schema

type family FromSchema (schema :: Schema) where
  FromSchema ('Schema schema) = schema

type IsSchema (schema :: Schema) =
  ( IsSchemaType (ToSchemaObject schema)
  , All IsSchemaObjectPair (FromSchema schema)
  )

showSchema :: forall (schema :: Schema). IsSchema schema => String
showSchema = showSchemaType @(ToSchemaObject schema)

data SchemaType
  = SchemaScalar Type
  | SchemaMaybe SchemaType
  | SchemaTry SchemaType -- ^ @since v1.2.0
  | SchemaList SchemaType
  | SchemaUnion [SchemaType] -- ^ @since v1.1.0
  | SchemaObject SchemaObjectMap

-- | Pretty show the given SchemaType.
showSchemaType :: forall (schema :: SchemaType). IsSchemaType schema => String
showSchemaType = SchemaShow.showSchemaType $ toSchemaTypeShow $ Proxy @schema

-- | The type-level analogue of 'Data.Aeson.Schema.Key.SchemaKey'.
data SchemaKey
  = NormalKey Symbol
  | PhantomKey Symbol

keyName :: forall key. KnownSymbol key => Text
keyName = Text.pack $ symbolVal $ Proxy @key

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
  toSchemaTypeShow :: Proxy schema -> SchemaShow.SchemaType

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
  toSchemaTypeShow _ = SchemaShow.SchemaScalar (tyConName $ typeRepTyCon $ typeRep $ Proxy @inner)

instance (IsSchemaType inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => IsSchemaType ('SchemaMaybe inner) where
  toSchemaTypeShow _ = SchemaShow.SchemaMaybe (toSchemaTypeShow $ Proxy @inner)

  parseValue path = \case
    Null -> return Nothing
    value -> (Just <$> parseValue @inner path value)

instance (IsSchemaType inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => IsSchemaType ('SchemaTry inner) where
  toSchemaTypeShow _ = SchemaShow.SchemaTry (toSchemaTypeShow $ Proxy @inner)

  parseValue path = wrapTry . parseValue @inner path
    where
      wrapTry parser = (Just <$> parser) <|> pure Nothing

instance (IsSchemaType inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => IsSchemaType ('SchemaList inner) where
  toSchemaTypeShow _ = SchemaShow.SchemaList (toSchemaTypeShow $ Proxy @inner)

  parseValue path = \case
    Array a -> traverse (parseValue @inner path) (toList a)
    value -> parseFail @('SchemaList inner) path value

instance
  ( All IsSchemaType schemas
  , Show (SchemaResult ('SchemaUnion schemas))
  , FromJSON (SchemaResult ('SchemaUnion schemas))
  , ToJSON (SchemaResult ('SchemaUnion schemas))
  ) => IsSchemaType ('SchemaUnion (schemas :: [SchemaType])) where
  toSchemaTypeShow _ = SchemaShow.SchemaUnion (mapAll @IsSchemaType @schemas toSchemaTypeShow)

instance All IsSchemaObjectPair pairs => IsSchemaType ('SchemaObject pairs) where
  toSchemaTypeShow _ = SchemaShow.SchemaObject (mapAll @IsSchemaObjectPair @pairs toSchemaTypeShowPair)

  parseValue path = \case
    Aeson.Object o -> UnsafeObject . HashMap.fromList <$> parseValueMap o
    value -> parseFail @('SchemaObject pairs) path value
    where
      parseValueMap :: Aeson.Object -> Parser [(Text, Dynamic)]
      parseValueMap o = sequence $ mapAll @IsSchemaObjectPair @pairs $ \proxy -> parseValuePair proxy path o

  toValue = Aeson.Object . toValueMap

  showValue o = "{" ++ intercalate ", " (map fromPair pairs) ++ "}"
    where
      fromPair (k, v) = k ++ ": " ++ v
      pairs = mapAll @IsSchemaObjectPair @pairs $ \proxy -> showValuePair proxy o

toValueMap :: forall pairs. All IsSchemaObjectPair pairs => Object ('Schema pairs) -> Aeson.Object
toValueMap o = HashMap.unions $ mapAll @IsSchemaObjectPair @pairs (\proxy -> toValuePair proxy o)

class IsSchemaObjectPair (a :: (SchemaKey, SchemaType)) where
  toSchemaTypeShowPair :: Proxy a -> (SchemaShow.SchemaKey, SchemaShow.SchemaType)
  parseValuePair :: Proxy a -> [Text] -> Aeson.Object -> Parser (Text, Dynamic)
  toValuePair :: Proxy a -> Object schema -> Aeson.Object
  showValuePair :: Proxy a -> Object schema -> (String, String)

instance
  ( KnownSymbol (FromSchemaKey key)
  , IsSchemaKey key
  , IsSchemaType inner
  , Typeable (SchemaResult inner)
  ) => IsSchemaObjectPair '(key, inner) where
  toSchemaTypeShowPair _ = (toSchemaKey @key, toSchemaTypeShow $ Proxy @inner)

  parseValuePair _ path o = do
    let key = fromSchemaKey @key
    inner <- parseValue @inner (key:path) $ getContext @key o
    return (key, toDyn inner)

  toValuePair _ o = toContext @key (toValue @inner val)
    where
      val = unsafeGetKey @inner (Proxy @(FromSchemaKey key)) o

  showValuePair _ o = (showSchemaKey @key, showValue @inner val)
    where
      val = unsafeGetKey @inner (Proxy @(FromSchemaKey key)) o

class IsSchemaKey (key :: SchemaKey) where
  type FromSchemaKey key :: Symbol
  toSchemaKey :: SchemaShow.SchemaKey
  fromSchemaKey :: Text
  -- | Given schema `{ key: innerSchema }` for JSON data `{ key: val1 }`, get the JSON
  -- Value that `innerSchema` should parse.
  getContext :: Aeson.Object -> Value
  -- | Given JSON data `val` adhering to `innerSchema`, get the JSON object that should be
  -- merged with the outer JSON object.
  toContext :: Value -> Aeson.Object
  showSchemaKey :: String

instance KnownSymbol key => IsSchemaKey ('NormalKey key) where
  type FromSchemaKey ('NormalKey key) = key
  toSchemaKey = SchemaShow.NormalKey (Text.unpack $ keyName @key)
  fromSchemaKey = keyName @key
  -- | `innerSchema` should parse `val1`
  getContext = HashMap.lookupDefault Null (keyName @key)
  -- | `val` should be inserted with key `key`
  toContext val = HashMap.singleton (keyName @key) val
  showSchemaKey = show (keyName @key)

instance KnownSymbol key => IsSchemaKey ('PhantomKey key) where
  type FromSchemaKey ('PhantomKey key) = key
  toSchemaKey = SchemaShow.PhantomKey (Text.unpack $ keyName @key)
  fromSchemaKey = keyName @key
  -- | `innerSchema` should parse the same object that `key` is in
  getContext = Aeson.Object
  -- | If `val` is an object, it should be merged with the JSON object.
  toContext = \case
    Aeson.Object o -> o
    -- `Try` schema could store `Nothing`, which would return `Null`. In this case, there is no
    -- context to merge
    Null -> mempty
    v -> unreachable $ "Invalid value for phantom key: " ++ show v
  showSchemaKey = Text.unpack $ Text.concat ["[", keyName @key, "]"]

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

{- Helpers -}

-- | An error function to indicate that a branch is unreachable. Provides a useful error message
-- if it ends up happening, pointing users to write a bug report.
unreachable :: String -> a
unreachable msg = error $ unlines
  [ "`aeson-schemas` internal error: " ++ msg
  , "Please file a bug report at https://github.com/LeapYear/aeson-schemas/issues/"
  ]
