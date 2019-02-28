{-|
Module      :  Data.Aeson.Schema.Internal
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Internal definitions for declaring JSON schemas.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Data.Aeson (FromJSON(..), Value(..))
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
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
import GHC.TypeLits
    (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

{- Schema-validated JSON object -}

-- | The object containing JSON data and its schema.
newtype Object (schema :: SchemaType) = UnsafeObject (HashMap Text Dynamic)
  deriving (Show)

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
  type SchemaResult schema

  parseValue :: [Text] -> Value -> Either String (SchemaResult schema)
  default parseValue :: FromJSON (SchemaResult schema) => [Text] -> Value -> Either String (SchemaResult schema)
  parseValue path value = first (const errMsg) $ parseEither parseJSON value
    where
      errMsg = mkErrMsg @schema path value

instance FromSchema 'SchemaBool where
  type SchemaResult 'SchemaBool = Bool

instance FromSchema 'SchemaInt where
  type SchemaResult 'SchemaInt = Int

instance FromSchema 'SchemaDouble where
  type SchemaResult 'SchemaDouble = Double

instance FromSchema 'SchemaText where
  type SchemaResult 'SchemaText = Text

instance (FromSchema inner, FromJSON (SchemaResult inner)) => FromSchema ('SchemaMaybe inner) where
  type SchemaResult ('SchemaMaybe inner) = Maybe (SchemaResult inner)

instance (FromSchema inner, FromJSON (SchemaResult inner)) => FromSchema ('SchemaList inner) where
  type SchemaResult ('SchemaList inner) = [SchemaResult inner]

instance FromSchema ('SchemaObject '[]) where
  type SchemaResult ('SchemaObject '[]) = Object ('SchemaObject '[])

  parseValue path = \case
    Object _ -> Right $ UnsafeObject mempty
    value -> Left $ mkErrMsg @('SchemaObject '[]) path value

instance
  ( KnownSymbol key
  , FromSchema inner
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
    _ -> Left $ mkErrMsg @('SchemaObject ('(key, inner) ': rest)) path value

mkErrMsg :: forall (schema :: SchemaType). Typeable schema => [Text] -> Value -> String
mkErrMsg path value =
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
