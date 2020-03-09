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
import Data.Bifunctor (second)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema.Internal (SchemaType(..))
import Data.Aeson.Schema.TH.Parse
import Data.Aeson.Schema.TH.Utils
    (schemaPairsToTypeQ, typeQListToTypeQ, typeToSchemaPairs)

-- | Defines a QuasiQuoter for writing schemas.
--
-- Example:
--
-- > import Data.Aeson.Schema (schema)
-- >
-- > type MySchema = [schema|
-- >   {
-- >     foo: {
-- >       a: Int,
-- >       // you can add comments like this
-- >       nodes: List {
-- >         b: Maybe Bool,
-- >       },
-- >       c: Text,
-- >       d: Text,
-- >       e: MyType,
-- >       f: Maybe List {
-- >         name: Text,
-- >       },
-- >     },
-- >   }
-- > |]
--
-- Syntax:
--
-- * @{ key: \<schema\>, ... }@ corresponds to a JSON 'Data.Aeson.Schema.Object' with the given key
--   mapping to the given schema.
--
-- * @Bool@, @Int@, @Double@, and @Text@ correspond to the usual Haskell values.
--
-- * @Maybe \<schema\>@ and @List \<schema\>@ correspond to @Maybe@ and @[]@, containing values
--   specified by the provided schema (no parentheses needed).
--
-- * @\<schema1\> | \<schema2\>@ corresponds to a JSON value that matches one of the given schemas.
--   When extracted from an 'Data.Aeson.Schema.Object', it deserializes into a
--   'Data.Aeson.Schema.Utils.Sum.JSONSum' object.
--
-- * Any other uppercase identifier corresponds to the respective type in scope -- requires a
--   FromJSON instance.
--
-- * @{ key: #Other, ... }@ maps the given key to the @Other@ schema.
--
-- * @{ #Other, ... }@ extends this schema with the @Other@ schema.
schema :: QuasiQuoter
schema = QuasiQuoter
  { quoteExp = error "Cannot use `schema` for Exp"
  , quoteDec = error "Cannot use `schema` for Dec"
  , quoteType = parse schemaDef >=> \case
      SchemaDefObj items -> generateSchemaObject items
      _ -> fail "`schema` definition must be an object"
  , quotePat = error "Cannot use `schema` for Pat"
  }

generateSchemaObject :: [SchemaDefObjItem] -> TypeQ
generateSchemaObject items = [t| 'SchemaObject $(fromItems items) |]
  where
    fromItems = schemaPairsToTypeQ <=< resolveParts . concat <=< mapM toParts

generateSchema :: SchemaDef -> TypeQ
generateSchema = \case
  SchemaDefType "Bool"   -> [t| 'SchemaBool |]
  SchemaDefType "Int"    -> [t| 'SchemaInt |]
  SchemaDefType "Double" -> [t| 'SchemaDouble |]
  SchemaDefType "Text"   -> [t| 'SchemaText |]
  SchemaDefType other    -> [t| 'SchemaCustom $(getType other) |]
  SchemaDefMaybe inner   -> [t| 'SchemaMaybe $(generateSchema inner) |]
  SchemaDefList inner    -> [t| 'SchemaList $(generateSchema inner) |]
  SchemaDefInclude other -> getType other
  SchemaDefObj items     -> generateSchemaObject items
  SchemaDefUnion schemas -> [t| 'SchemaUnion $(typeQListToTypeQ $ map generateSchema schemas) |]

{- Helpers -}

getName :: String -> Q Name
getName ty = maybe (fail $ "Unknown type: " ++ ty) return =<< lookupTypeName ty

getType :: String -> TypeQ
getType = getName >=> conT

data KeySource = Provided | Imported
  deriving (Show,Eq)

-- | Parse SchemaDefObjItem into a list of tuples, each containing a key to add to the schema,
-- the value for the key, and the source of the key.
toParts :: SchemaDefObjItem -> Q [(String, TypeQ, KeySource)]
toParts = \case
  SchemaDefObjPair (k, v) -> pure . tagAs Provided $ [(k, generateSchema v)]
  SchemaDefObjExtend other -> do
    name <- getName other
    reify name >>= \case
      TyConI (TySynD _ _ (AppT (PromotedT ty) inner)) | ty == 'SchemaObject ->
        pure . tagAs Imported . map (second pure) $ typeToSchemaPairs inner
      _ -> fail $ "'" ++ show name ++ "' is not a SchemaObject"
  where
    tagAs source = map $ \(k,v) -> (k,v,source)

-- | Resolve the parts returned by 'toParts' as such:
--
-- 1. Any explicitly provided keys shadow/overwrite imported keys
-- 2. Fail if duplicate keys are both explicitly provided
-- 3. Fail if duplicate keys are both imported
resolveParts :: [(String, TypeQ, KeySource)] -> Q [(String, TypeQ)]
resolveParts parts = do
  resolved <- resolveParts' $ HashMap.fromListWith (++) $ map nameAndSource parts
  return $ mapMaybe (alignWithResolved resolved) parts
  where
    nameAndSource (name, _, source) = (name, [source])
    resolveParts' = HashMap.traverseWithKey $ \name sources -> do
      -- invariant: length sources > 0
      let numOf source = length $ filter (== source) sources
      case (numOf Provided, numOf Imported) of
        (1, _) -> return Provided
        (0, 1) -> return Imported
        (x, _) | x > 1 -> fail $ "Key '" ++ name ++ "' specified multiple times"
        (_, x) | x > 1 -> fail $ "Key '" ++ name ++ "' declared in multiple imported schemas"
        _ -> fail "Broken invariant in resolveParts"
    alignWithResolved resolved (name, ty, source) =
      let resolvedSource = resolved HashMap.! name
      in if resolvedSource == source
        then Just (name, ty)
        else Nothing
