{-|
Module      :  Data.Aeson.Schema.TH.Schema
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'schema' quasiquoter.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aeson.Schema.TH.Schema (schema) where

import Control.Monad ((<=<), (>=>))
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema.Internal (SchemaType(..))
import Data.Aeson.Schema.TH.Parse

-- | Defines a QuasiQuoter for writing schemas.
--
-- Example:
--
-- > import Data.Aeson.Schema (SchemaType(..))
-- > import Data.Aeson.Schema.TH (schema)
-- >
-- > -- | MySchema ~ 'SchemaObject
-- > --     '[ '("a", 'SchemaInt)
-- > --      , '("nodes", 'SchemaMaybe (SchemaList ('SchemaObject
-- > --           '[ '("b", 'SchemaMaybe 'SchemaBool)
-- > --            ]
-- > --         )))
-- > --      , '("c", 'SchemaText)
-- > --      , '("d", 'SchemaText)
-- > --      , '("e", 'SchemaCustom "MyType")
-- > --      ]
-- > type MySchema = [schema|
-- >   {
-- >     "foo": {
-- >        "a": Int,
-- >        "nodes": Maybe List {
-- >           "b": Maybe Bool,
-- >        },
-- >        "c": Text,
-- >        "d": Text,
-- >        "e": MyType,
-- >     },
-- >   }
-- > |]
--
-- The schema definition accepts the following syntax:
--
-- * @Bool@ corresponds to @'SchemaBool@, and similarly for @Int@, @Double@, and @Text@
--
-- * @Maybe x@ and @List x@ correspond to @'SchemaMaybe x@ and @'SchemaList x@, respectively. (no
--   parentheses needed)
--
-- * Any other uppercase identifier corresponds to @'SchemaCustom ident@
--
-- * @{ "key": schema, ... }@ corresponds to @'SchemaObject '[ '("key", schema), ... ]@
--
-- * @{ "key": #Other }@ includes @Other@ as a schema
--
-- * @{ "key": schema, #Other }@ extends this schema with @Other@
schema :: QuasiQuoter
schema = QuasiQuoter
  { quoteExp = error "Cannot use `schema` for Exp"
  , quoteDec = error "Cannot use `schema` for Dec"
  , quoteType = parse schemaDef >=> generateSchema
  , quotePat = error "Cannot use `schema` for Pat"
  }

generateSchema :: SchemaDef -> TypeQ
generateSchema = \case
  SchemaDefType "Bool"       -> [t| 'SchemaBool |]
  SchemaDefType "Int"        -> [t| 'SchemaInt |]
  SchemaDefType "Double"     -> [t| 'SchemaDouble |]
  SchemaDefType "Text"       -> [t| 'SchemaText |]
  SchemaDefType other        -> [t| 'SchemaCustom $(getType other) |]
  SchemaDefMod "Maybe" inner -> [t| 'SchemaMaybe $(generateSchema inner) |]
  SchemaDefMod "List" inner  -> [t| 'SchemaList $(generateSchema inner) |]
  SchemaDefMod other _       -> fail $ "Invalid schema modification: " ++ other
  SchemaDefInclude other     -> getType other
  SchemaDefObj items         -> [t| 'SchemaObject $(fromItems items) |]

{- Helpers -}

getName :: String -> Q Name
getName ty = maybe (fail $ "Unknown type: " ++ ty) return =<< lookupTypeName ty

getType :: String -> TypeQ
getType = getName >=> conT

-- | Parse a list of SchemaDefObjItems into a a type-level list for 'SchemaObject.
fromItems :: [SchemaDefObjItem] -> TypeQ
fromItems = toTypeList . concat <=< mapM toParts
  where
    pairT (a, b) = [t| '($a, $b) |]
    toParts = \case
      SchemaDefObjPair (k, v) -> pure [pairT (litT $ strTyLit k, generateSchema v)]
      SchemaDefObjExtend other -> do
        name <- getName other
        reify name >>= \case
          TyConI (TySynD _ _ (AppT (PromotedT ty) inner)) | ty == 'SchemaObject -> pure $ fromTypeList inner
          _ -> fail $ "'" ++ show name ++ "' is not a SchemaObject"

fromTypeList :: Type -> [TypeQ]
fromTypeList = \case
  PromotedNilT -> []
  AppT (AppT PromotedConsT x) xs -> pure x : fromTypeList xs
  SigT ty _ -> fromTypeList ty
  ty -> error $ "Not a type-level list: " ++ show ty

toTypeList :: [TypeQ] -> TypeQ
toTypeList = foldr consT promotedNilT
  where
    consT x xs = [t| $x ': $xs |]
