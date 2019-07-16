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
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema.Internal (SchemaType(..))
import Data.Aeson.Schema.TH.Parse
import Data.Aeson.Schema.TH.Utils (fromTypeList, toTypeList)

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
-- >     foo: {
-- >       a: Int,
-- >       // you can add comments like this
-- >       nodes: Maybe List {
-- >         b: Maybe Bool,
-- >       },
-- >       c: Text,
-- >       d: Text,
-- >       e: MyType,
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
  , quoteType = parse schemaDef >=> \case
      SchemaDefObj items -> generateSchemaObject items
      _ -> fail "`schema` definition must be an object"
  , quotePat = error "Cannot use `schema` for Pat"
  }

generateSchemaObject :: [SchemaDefObjItem] -> TypeQ
generateSchemaObject items = [t| 'SchemaObject $(fromItems items) |]
  where
    fromItems = toTypeList <=< resolveParts . concat <=< mapM toParts

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

{- Helpers -}

getName :: String -> Q Name
getName ty = maybe (fail $ "Unknown type: " ++ ty) return =<< lookupTypeName ty

getType :: String -> TypeQ
getType = getName >=> conT

data KeySource = Provided | Imported
  deriving (Show,Eq)

-- | Parse SchemaDefObjItem into a list of tuples, each containing the key to add to the schema,
-- the value for the key, and the source of the key.
toParts :: SchemaDefObjItem -> Q [(String, TypeQ, KeySource)]
toParts = \case
  SchemaDefObjPair (k, v) -> pure . tagAs Provided $ [(k, generateSchema v)]
  SchemaDefObjExtend other -> do
    name <- getName other
    reify name >>= \case
      TyConI (TySynD _ _ (AppT (PromotedT ty) inner)) | ty == 'SchemaObject ->
        tagAs Imported <$> fromTypeList inner
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
