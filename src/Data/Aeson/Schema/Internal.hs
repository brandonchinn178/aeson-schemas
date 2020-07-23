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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Bifunctor (first)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, splitTyConApp, tyConName, typeRep, typeRepTyCon)
import Fcf (type (<=<), type (=<<))
import qualified Fcf
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
newtype Object (schema :: Schema) = UnsafeObject (HashMap Text Dynamic)

instance IsSchemaType ('SchemaObject schema) => Show (Object ('Schema schema)) where
  show = showValue @('SchemaObject schema)

instance IsSchemaType ('SchemaObject schema) => FromJSON (Object ('Schema schema)) where
  parseJSON = parseValue @('SchemaObject schema) []

instance IsSchemaType ('SchemaObject schema) => ToJSON (Object ('Schema schema)) where
  toJSON = toValue @('SchemaObject schema)

toMap :: forall schema. IsSchemaObjectMap schema => Object ('Schema schema) -> Aeson.Object
toMap = toValueMap @schema

{- Type-level schema definitions -}

type SchemaObjectMap = [(SchemaKey, SchemaType)]

-- | The type-level schema definition for JSON data.
--
-- To view a schema for debugging, use 'showSchema'.
data Schema = Schema SchemaObjectMap

type family ToSchemaObject (schema :: Schema) where
  ToSchemaObject ('Schema inner) = 'SchemaObject inner

data SchemaType
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaCustom Type
  | SchemaMaybe SchemaType
  | SchemaTry SchemaType -- ^ @since v1.2.0
  | SchemaList SchemaType
  | SchemaObject SchemaObjectMap
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
      ("'SchemaTry", [inner]) -> SchemaShow.SchemaTry $ cast inner
      ("'SchemaList", [inner]) -> SchemaShow.SchemaList $ cast inner
      ("'SchemaObject", [pairs]) -> SchemaShow.SchemaObject $ map getSchemaObjectPair $ typeRepToList pairs
      ("'SchemaUnion", [schemas]) -> SchemaShow.SchemaUnion $ map cast $ typeRepToList schemas
      _ -> unreachable $ "Unknown schema type: " ++ show tyRep

    getSchemaObjectPair tyRep =
      let (key, val) = typeRepToPair tyRep
          fromTypeRep = tail . init . typeRepName -- strip leading + trailing quote
          schemaKey = case splitTypeRep key of
            ("'NormalKey", [key']) -> SchemaShow.NormalKey $ fromTypeRep key'
            ("'PhantomKey", [key']) -> SchemaShow.PhantomKey $ fromTypeRep key'
            _ -> unreachable $ "Unknown schema key: " ++ show key
      in (schemaKey, cast val)

    typeRepToPair tyRep = case splitTypeRep tyRep of
      ("'(,)", [a, b]) -> (a, b)
      _ -> unreachable $ "Unknown pair: " ++ show tyRep

    typeRepToList tyRep = case splitTypeRep tyRep of
      ("'[]", []) -> []
      ("':", [x, rest]) -> x : typeRepToList rest
      _ -> unreachable $ "Unknown list: " ++ show tyRep

    splitTypeRep = first tyConName . splitTyConApp
    typeRepName = tyConName . typeRepTyCon

-- | Pretty show the given SchemaType.
showSchemaType :: forall (a :: SchemaType). Typeable a => String
showSchemaType = SchemaShow.showSchemaType $ toSchemaTypeShow @a

-- | The type-level analogue of 'Data.Aeson.Schema.Key.SchemaKey'.
data SchemaKey
  = NormalKey Symbol
  | PhantomKey Symbol

keyName :: forall key. KnownSymbol key => Text
keyName = Text.pack $ symbolVal $ Proxy @key

{- Conversions from schema types into Haskell types -}

-- | A type family mapping SchemaType to the corresponding Haskell type.
type family SchemaResult (schema :: SchemaType) where
  SchemaResult 'SchemaBool = Bool
  SchemaResult 'SchemaInt = Int
  SchemaResult 'SchemaDouble = Double
  SchemaResult 'SchemaText = Text
  SchemaResult ('SchemaCustom inner) = inner
  SchemaResult ('SchemaMaybe inner) = Maybe (SchemaResult inner)
  SchemaResult ('SchemaTry inner) = Maybe (SchemaResult inner)
  SchemaResult ('SchemaList inner) = [SchemaResult inner]
  SchemaResult ('SchemaObject inner) = Object ('Schema inner)
  SchemaResult ('SchemaUnion schemas) = SumType (SchemaResultList schemas)

type family SchemaResultList (xs :: [SchemaType]) where
  SchemaResultList '[] = '[]
  SchemaResultList (x ': xs) = SchemaResult x ': SchemaResultList xs

-- | A type-class for types that can be parsed from JSON for an associated schema type.
class Typeable schema => IsSchemaType (schema :: SchemaType) where
  parseValue :: [Text] -> Value -> Parser (SchemaResult schema)
  default parseValue :: FromJSON (SchemaResult schema) => [Text] -> Value -> Parser (SchemaResult schema)
  parseValue path value = parseJSON value <|> parseFail @schema path value

  toValue :: SchemaResult schema -> Value
  default toValue :: ToJSON (SchemaResult schema) => SchemaResult schema -> Value
  toValue = toJSON

  showValue :: SchemaResult schema -> String
  default showValue :: Show (SchemaResult schema) => SchemaResult schema -> String
  showValue = show

instance IsSchemaType 'SchemaBool

instance IsSchemaType 'SchemaInt

instance IsSchemaType 'SchemaDouble

instance IsSchemaType 'SchemaText

instance (Show inner, Typeable inner, FromJSON inner, ToJSON inner) => IsSchemaType ('SchemaCustom inner)

instance (IsSchemaType inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => IsSchemaType ('SchemaMaybe inner) where
  parseValue path = \case
    Null -> return Nothing
    value -> (Just <$> parseValue @inner path value)

instance (IsSchemaType inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => IsSchemaType ('SchemaTry inner) where
  parseValue path = wrapTry . parseValue @inner path
    where
      wrapTry parser = (Just <$> parser) <|> pure Nothing

instance (IsSchemaType inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => IsSchemaType ('SchemaList inner) where
  parseValue path value = case value of
    Array a -> traverse (parseValue @inner path) (toList a)
    _ -> parseFail @('SchemaList inner) path value

instance (Typeable schema, IsSchemaObjectMap schema) => IsSchemaType ('SchemaObject schema) where
  parseValue path = \case
    Object o -> UnsafeObject <$> parseValueMap @schema path o
    value -> parseFail @('SchemaObject schema) path value

  toValue = Object . toValueMap @schema

  showValue o =
    let pairs = showValueMap @schema o
    in "{" ++ intercalate ", " (map fromPair pairs) ++ "}"
    where
      fromPair (k, v) = k ++ ": " ++ v

class IsSchemaObjectMap (a :: SchemaObjectMap) where
  parseValueMap :: [Text] -> HashMap Text Value -> Parser (HashMap Text Dynamic)
  toValueMap :: Object schema -> Aeson.Object
  showValueMap :: Object schema -> [(String, String)]

instance IsSchemaObjectMap '[] where
  parseValueMap _ _ = return mempty
  toValueMap _ = mempty
  showValueMap _ = []

instance
  ( KnownSymbol (FromSchemaKey key)
  , IsSchemaKey key
  , IsSchemaType inner
  , Typeable (SchemaResult inner)
  , IsSchemaObjectMap rest
  ) => IsSchemaObjectMap ( '(key, inner) ': rest ) where

  parseValueMap path o = do
    let key = fromSchemaKey @key

    inner <- parseValue @inner (key:path) $ getContext @key o
    rest <- parseValueMap @rest path o

    return $ HashMap.insert key (toDyn inner) rest

  toValueMap o =
    let val = toValue @inner $ unsafeGetKey @inner (Proxy @(FromSchemaKey key)) o
        rest = toValueMap @rest o
    in buildContext @key val rest

  showValueMap o =
    let key = showSchemaKey @key
        val = showValue @inner $ unsafeGetKey @inner (Proxy @(FromSchemaKey key)) o
        rest = showValueMap @rest o
    in (key, val) : rest

class IsSchemaKey (key :: SchemaKey) where
  type FromSchemaKey key :: Symbol
  fromSchemaKey :: Text
  getContext :: HashMap Text Value -> Value
  buildContext :: Value -> (Aeson.Object -> Aeson.Object)
  showSchemaKey :: String

instance KnownSymbol key => IsSchemaKey ('NormalKey key) where
  type FromSchemaKey ('NormalKey key) = key
  fromSchemaKey = keyName @key
  getContext = HashMap.lookupDefault Null (keyName @key)
  buildContext val = HashMap.insert (keyName @key) val
  showSchemaKey = show (keyName @key)

instance KnownSymbol key => IsSchemaKey ('PhantomKey key) where
  type FromSchemaKey ('PhantomKey key) = key
  fromSchemaKey = keyName @key
  getContext = Object
  buildContext val = HashMap.union context
    where
      context = case val of
        Object o -> o
        _ -> unreachable $ "Invalid value for phantom key: " ++ show val
  showSchemaKey = Text.unpack $ Text.concat ["[", keyName @key, "]"]

instance
  ( All IsSchemaType schemas
  , Typeable schemas
  , Show (SchemaResult ('SchemaUnion schemas))
  , FromJSON (SchemaResult ('SchemaUnion schemas))
  , ToJSON (SchemaResult ('SchemaUnion schemas))
  ) => IsSchemaType ('SchemaUnion schemas)

-- | A helper for creating fail messages when parsing a schema.
parseFail :: forall (schema :: SchemaType) m a. (MonadFail m, Typeable schema) => [Text] -> Value -> m a
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
