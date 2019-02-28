{-|
Module      :  Data.Aeson.Schema.Internal
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Internal definitions for declaring JSON schemas.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Data.Aeson (FromJSON(..), Value(..))
import Data.Aeson.Types (Parser)
import Data.Dynamic (Dynamic, fromDyn, toDyn)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, splitTyConApp, tyConName, typeRep, typeRepTyCon)
import Fcf (type (<=<), type (=<<), Eval, Find, FromMaybe, Fst, Snd, TyEq)
import GHC.Exts (toList)
import GHC.TypeLits
    (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

{- Schema-validated JSON object -}

-- | The object containing JSON data and its schema.
--
-- Has a 'FromJSON' instance, so you can use the usual 'Data.Aeson' decoding functions.
--
-- > obj = decode "{\"a\": 1}" :: Maybe (Object ('SchemaObject '[ '("a", 'SchemaInt) ]))
newtype Object (schema :: SchemaType) = UnsafeObject (HashMap Text Dynamic)

-- | A constraint that checks if the given schema is a 'SchemaObject.
type IsSchemaObject schema = (FromSchema schema, SchemaResult schema ~ Object schema)

instance IsSchemaObject schema => Show (Object schema) where
  show = showValue @schema

instance IsSchemaObject schema => FromJSON (Object schema) where
  parseJSON = parseValue @schema []

{- Type-level schema definitions -}

-- | The schema definition for JSON data.
data SchemaGraph s
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaCustom s
  | SchemaMaybe (SchemaGraph s)
  | SchemaList (SchemaGraph s)
  | SchemaObject [(s, SchemaGraph s)]
  deriving (Show)

-- | A 'SchemaGraph' at the kind level.
type SchemaType = SchemaGraph Symbol

-- | Pretty show the given SchemaType.
prettyShow :: forall (a :: SchemaType). Typeable a => String
prettyShow = showSchemaType $ typeRep (Proxy @a)
  where
    showSchemaType tyRep = case splitTypeRep tyRep of
      ("'SchemaObject", [pairs]) ->
        unwords ["SchemaObject", showPairs $ getSchemaObjectPairs pairs]
      (con, args) | "'Schema" `isPrefixOf` con ->
        unwords $ tail con : map (wrap . showSchemaType) args
      (con, _) -> con
    getSchemaObjectPairs tyRep = case splitTypeRep tyRep of
      ("'[]", []) -> []
      ("':", [x, rest]) -> case splitTypeRep x of
        ("'(,)", [key, val]) -> (typeRepName key, typeRepName val) : getSchemaObjectPairs rest
        _ -> error $ "Unknown pair when showing SchemaType: " ++ show x
      _ -> error $ "Unknown list when showing SchemaType: " ++ show tyRep
    showPairs l =
      let showPair (a, b) = "(" ++ a ++ ", " ++ b ++ ")"
      in "[" ++ intercalate ", " (map showPair l) ++ "]"
    wrap s = if ' ' `elem` s then "(" ++ s ++ ")" else s
    splitTypeRep rep =
      let (con, args) = splitTyConApp rep
      in (tyConName con, args)
    typeRepName = tyConName . typeRepTyCon

{- Conversions from schema types into Haskell types -}

-- | A type-class for types that can be parsed from JSON for an associated schema type.
class Typeable schema => FromSchema (schema :: SchemaType) where
  type SchemaResult schema = result | result -> schema

  parseValue :: [Text] -> Value -> Parser (SchemaResult schema)
  default parseValue :: FromJSON (SchemaResult schema) => [Text] -> Value -> Parser (SchemaResult schema)
  parseValue path value = parseJSON value <|> parseFail @schema path value

  showValue :: SchemaResult schema -> String
  default showValue :: Show (SchemaResult schema) => SchemaResult schema -> String
  showValue = show

instance FromSchema 'SchemaBool where
  type SchemaResult 'SchemaBool = Bool

instance FromSchema 'SchemaInt where
  type SchemaResult 'SchemaInt = Int

instance FromSchema 'SchemaDouble where
  type SchemaResult 'SchemaDouble = Double

instance FromSchema 'SchemaText where
  type SchemaResult 'SchemaText = Text

instance (FromSchema inner, Show (SchemaResult inner)) => FromSchema ('SchemaMaybe inner) where
  type SchemaResult ('SchemaMaybe inner) = Maybe (SchemaResult inner)

  parseValue path = \case
    Null -> return Nothing
    value -> (Just <$> parseValue @inner path value) <|> parseFail @('SchemaMaybe inner) path value

instance (FromSchema inner, Show (SchemaResult inner)) => FromSchema ('SchemaList inner) where
  type SchemaResult ('SchemaList inner) = [SchemaResult inner]

  parseValue path value = case value of
    Array a -> traverse (parseValue @inner path) (toList a) <|> fail'
    _ -> fail'
    where
      fail' = parseFail @('SchemaList inner) path value

instance FromSchema ('SchemaObject '[]) where
  type SchemaResult ('SchemaObject '[]) = Object ('SchemaObject '[])

  parseValue path = \case
    Object _ -> return $ UnsafeObject mempty
    value -> parseFail @('SchemaObject '[]) path value

  showValue _ = "{}"

instance
  ( KnownSymbol key
  , FromSchema inner
  , Show (SchemaResult inner)
  , Typeable (SchemaResult inner)
  , FromSchema ('SchemaObject rest)
  , SchemaResult ('SchemaObject rest) ~ Object ('SchemaObject rest)
  , Typeable rest
  ) => FromSchema ('SchemaObject ('(key, inner) ': rest)) where
  type SchemaResult ('SchemaObject ('(key, inner) ': rest)) = Object ('SchemaObject ('(key, inner) ': rest))

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
      value = fromDyn @(SchemaResult inner) (hm ! Text.pack key) $ error $ "Could not load key: " ++ key
      pair = "\"" ++ key ++ "\": " ++ show value

parseFail :: forall (schema :: SchemaType) m a. (Monad m, Typeable schema) => [Text] -> Value -> m a
parseFail path value = fail $
  "Could not parse " ++ quotedValue ++ path' ++ " with schema: " ++ prettyShow @schema
  where
    quotedValue = "`" ++ show value ++ "`"
    path' = if null path
      then ""
      else " at path '" ++ Text.unpack (Text.intercalate "." $ reverse path) ++ "'"

{- Lookups within SchemaObject -}

-- | The type-level function that return the schema of the given key in a 'SchemaObject'.
type family LookupSchema (key :: Symbol) (schema :: SchemaType) :: SchemaType where
  LookupSchema key ('SchemaObject schema) = Eval
    ( Snd
    =<< FromMaybe (TypeError
      (     'Text "Key '"
      ':<>: 'Text key
      ':<>: 'Text "' does not exist in the following schema:"
      ':$$: 'ShowType schema
      ))
    =<< Find (TyEq key <=< Fst) schema
    )
  LookupSchema key schema = TypeError
    (     'Text "Attempted to lookup key '"
    ':<>: 'Text key
    ':<>: 'Text "' in the following schema:"
    ':$$: 'ShowType schema
    )

-- | Get a key from the given 'Object', returned as the type encoded in its schema.
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
