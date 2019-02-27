{-|
Module      :  Data.Aeson.Schema.Internal
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Internal definitions for declaring JSON schemas.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Schema.Internal where

import Data.Aeson (FromJSON(..))
import Data.Dynamic (Dynamic, fromDyn)
import Data.HashMap.Strict (HashMap, (!))
import Data.List (intercalate, isPrefixOf)
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
class HasSchema result where
  type ToSchema result = (schema :: SchemaType) | schema -> result

instance HasSchema Bool where
  type ToSchema Bool = 'SchemaBool

instance HasSchema Int where
  type ToSchema Int = 'SchemaInt

instance HasSchema Double where
  type ToSchema Double = 'SchemaDouble

instance HasSchema Text.Text where
  type ToSchema Text.Text = 'SchemaText

instance HasSchema inner => HasSchema (Maybe inner) where
  type ToSchema (Maybe inner) = 'SchemaMaybe (ToSchema inner)

instance HasSchema inner => HasSchema [inner] where
  type ToSchema [inner] = 'SchemaList (ToSchema inner)

instance HasSchema (Object ('SchemaObject inner)) where
  type ToSchema (Object ('SchemaObject inner)) = 'SchemaObject inner

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
   . ( endSchema ~ LookupSchema key schema    -- lookup key in schema for resulting schema
     , ToSchema result ~ endSchema            -- the final result type's associated schema should
                                              -- match resulting schema
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
