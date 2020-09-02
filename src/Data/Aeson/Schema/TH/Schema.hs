{-|
Module      :  Data.Aeson.Schema.TH.Schema
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'schema' quasiquoter.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Aeson.Schema.TH.Schema (schema) where

import Control.Monad (forM, unless, (<=<), (>=>))
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import Data.List (nubBy)
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema.Internal (Object)
import Data.Aeson.Schema.Key (SchemaKey'(..), SchemaKeyV, fromSchemaKeyV)
import Data.Aeson.Schema.TH.Parse
    (SchemaDef(..), SchemaDefObjItem(..), SchemaDefObjKey(..), parseSchemaDef)
import Data.Aeson.Schema.TH.Utils (TypeWithoutKinds, stripKinds)
import Data.Aeson.Schema.Type
    ( Schema'(..)
    , SchemaObjectMapV
    , SchemaType'(..)
    , SchemaTypeV
    , SchemaV
    , fromSchemaV
    , showSchemaTypeV
    , toSchemaObjectV
    )
import Data.Aeson.Schema.Utils.Invariant (unreachable)

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
-- * @Try \<schema\>@ corresponds to @Maybe@, where the value will be @Just@ if the given schema
--   successfully parses the value, or @Nothing@ otherwise. Different from @Maybe \<schema\>@,
--   where parsing @{ "foo": true }@ with @{ foo: Try Int }@ returns @Nothing@, whereas it would
--   be a parse error with @{ foo: Maybe Int }@ (added in v1.2.0)
--
-- * Any other uppercase identifier corresponds to the respective type in scope -- requires a
--   FromJSON instance.
--
-- Advanced syntax:
--
-- * @\<schema1\> | \<schema2\>@ corresponds to a JSON value that matches one of the given schemas.
--   When extracted from an 'Data.Aeson.Schema.Object', it deserializes into a
--   'Data.Aeson.Schema.Utils.Sum.JSONSum' object. (added in v1.1.0)
--
-- * @{ [key]: \<schema\> }@ uses the current object to resolve the keys in the given schema. Only
--   object schemas are allowed here. (added in v1.2.0)
--
-- * @{ key: #Other, ... }@ maps the given key to the @Other@ schema. The @Other@ schema needs to
--   be defined in another module.
--
-- * @{ #Other, ... }@ extends this schema with the @Other@ schema. The @Other@ schema needs to
--   be defined in another module.
schema :: QuasiQuoter
schema = QuasiQuoter
  { quoteExp = error "Cannot use `schema` for Exp"
  , quoteDec = error "Cannot use `schema` for Dec"
  , quoteType = parseSchemaDef >=> \case
      SchemaDefObj items -> generateSchemaObject items
      _ -> fail "`schema` definition must be an object"
  , quotePat = error "Cannot use `schema` for Pat"
  }
  where
    generateSchemaObject items = schemaVToTypeQ . Schema =<< generateSchemaObjectV items

schemaVToTypeQ :: SchemaV -> TypeQ
schemaVToTypeQ = appT [t| 'Schema |] . schemaObjectMapVToTypeQ . fromSchemaV

schemaObjectMapVToTypeQ :: SchemaObjectMapV -> TypeQ
schemaObjectMapVToTypeQ = promotedListT . map schemaObjectPairVToTypeQ
  where
    schemaObjectPairVToTypeQ :: (SchemaKeyV, SchemaTypeV) -> TypeQ
    schemaObjectPairVToTypeQ = promotedPairT . bimap schemaKeyVToTypeQ schemaTypeVToTypeQ

    schemaKeyVToTypeQ :: SchemaKeyV -> TypeQ
    schemaKeyVToTypeQ = \case
      NormalKey key  -> [t| 'NormalKey $(litT $ strTyLit key) |]
      PhantomKey key -> [t| 'PhantomKey $(litT $ strTyLit key) |]

schemaTypeVToTypeQ :: SchemaTypeV -> TypeQ
schemaTypeVToTypeQ = \case
  -- some hardcoded cases
  SchemaScalar "Bool"   -> [t| 'SchemaScalar Bool |]
  SchemaScalar "Int"    -> [t| 'SchemaScalar Int |]
  SchemaScalar "Double" -> [t| 'SchemaScalar Double |]
  SchemaScalar "Text"   -> [t| 'SchemaScalar Text |]
  SchemaScalar other    -> [t| 'SchemaScalar $(getType other) |]
  SchemaMaybe inner     -> [t| 'SchemaMaybe $(schemaTypeVToTypeQ inner) |]
  SchemaTry inner       -> [t| 'SchemaTry $(schemaTypeVToTypeQ inner) |]
  SchemaList inner      -> [t| 'SchemaList $(schemaTypeVToTypeQ inner) |]
  SchemaUnion schemas   -> [t| 'SchemaUnion $(promotedListT $ map schemaTypeVToTypeQ schemas) |]
  SchemaObject pairs    -> [t| 'SchemaObject $(schemaObjectMapVToTypeQ pairs) |]
  where
    getType :: String -> TypeQ
    getType ty = maybe (fail $ "Unknown type: " ++ ty) conT =<< lookupTypeName ty

promotedListT :: [TypeQ] -> TypeQ
promotedListT = foldr consT promotedNilT
  where
    -- nb. https://stackoverflow.com/a/34457936
    consT x xs = appT (appT promotedConsT x) xs

promotedPairT :: (TypeQ, TypeQ) -> TypeQ
promotedPairT (a, b) = [t| '( $a, $b ) |]

data KeySource = Provided | Imported
  deriving (Show, Eq)

generateSchemaObjectV :: [SchemaDefObjItem] -> Q SchemaObjectMapV
generateSchemaObjectV schemaDefObjItems = do
  schemaObjectMapsWithSource <- mapM getSchemaObjectMap schemaDefObjItems

  let schemaObjectMaps :: LookupMap SchemaKeyV (KeySource, SchemaTypeV)
      schemaObjectMaps = concatMap (uncurry distribute) schemaObjectMapsWithSource

  either fail return $ resolveKeys schemaObjectMaps

-- | Get the SchemaObjectMapV for the given SchemaDefObjItem, along with where the SchemaObjectMapV
-- came from.
getSchemaObjectMap :: SchemaDefObjItem -> Q (SchemaObjectMapV, KeySource)
getSchemaObjectMap = \case
  SchemaDefObjPair (schemaDefKey, schemaDefType) -> do
    let schemaKey = fromSchemaDefKey schemaDefKey
    schemaType <- fromSchemaDefType schemaDefType

    case schemaKey of
      PhantomKey _ ->
        unless (isValidPhantomSchema schemaType) $
          fail $ "Invalid schema for '" ++ fromSchemaKeyV schemaKey ++ "': " ++ showSchemaTypeV schemaType
      _ -> return ()

    return ([(schemaKey, schemaType)], Provided)

  SchemaDefObjExtend other -> do
    schemaV <- reifySchema other
    return (fromSchemaV schemaV, Imported)
  where
    -- should return true if it's at all possible to get a valid parse
    isValidPhantomSchema = \case
      SchemaMaybe inner -> isValidPhantomSchema inner
      SchemaTry _ -> True -- even if inner is a non-object schema, it'll still parse to be Nothing
      SchemaUnion schemas -> any isValidPhantomSchema schemas
      SchemaObject _ -> True
      _ -> False

-- | Resolve the given keys with the following rules:
--
-- 1. Any explicitly provided keys shadow/overwrite imported keys
-- 2. Fail if duplicate keys are both explicitly provided
-- 3. Fail if duplicate keys are both imported
resolveKeys :: Show a => LookupMap SchemaKeyV (KeySource, a) -> Either String (LookupMap SchemaKeyV a)
resolveKeys = mapM (uncurry resolveKey) . groupByKeyWith fromSchemaKeyV
  where
    resolveKey key sourcesAndVals =
      let filterSource source = map snd $ filter ((== source) . fst) sourcesAndVals
          numProvided = length $ filterSource Provided
          numImported = length $ filterSource Imported
      in if
        | numProvided > 1 -> Left $ "Key '" ++ fromSchemaKeyV key ++ "' specified multiple times"
        | [val] <- filterSource Provided -> Right (key, val)
        | numImported > 1 -> Left $ "Key '" ++ fromSchemaKeyV key ++ "' declared in multiple imported schemas"
        | [val] <- filterSource Imported -> Right (key, val)
        | otherwise -> unreachable $ "resolveKey received: " ++ show (key, sourcesAndVals)

{- SchemaDef conversions -}

fromSchemaDefKey :: SchemaDefObjKey -> SchemaKeyV
fromSchemaDefKey = \case
  SchemaDefObjKeyNormal key -> NormalKey key
  SchemaDefObjKeyPhantom key -> PhantomKey key

fromSchemaDefType :: SchemaDef -> Q SchemaTypeV
fromSchemaDefType = \case
  SchemaDefType other    -> return $ SchemaScalar other
  SchemaDefMaybe inner   -> SchemaMaybe <$> fromSchemaDefType inner
  SchemaDefTry inner     -> SchemaTry <$> fromSchemaDefType inner
  SchemaDefList inner    -> SchemaList <$> fromSchemaDefType inner
  SchemaDefInclude other -> toSchemaObjectV <$> reifySchema other
  SchemaDefUnion schemas -> SchemaUnion <$> mapM fromSchemaDefType schemas
  SchemaDefObj items     -> SchemaObject <$> generateSchemaObjectV items

{- Template Haskell parsing -}

reifySchema :: String -> Q SchemaV
reifySchema = parseSchema <=< reifySchemaName <=< lookupSchemaName
  where
    lookupSchemaName :: String -> Q Name
    lookupSchemaName name = maybe (fail $ "Unknown schema: " ++ name) return =<< lookupTypeName name

    reifySchemaName :: Name -> Q TypeWithoutKinds
    reifySchemaName schemaName = reify schemaName >>= \case
      -- reify `type MySchema = 'Schema '[ ... ]`
      TyConI (TySynD _ _ (stripKinds -> ty))
        | AppT (PromotedT name) _ <- ty
        , name == 'Schema
        -> return ty

      -- reify `type MySchema = Object OtherSchema`
      TyConI (TySynD _ _ (stripKinds -> ty))
        | AppT (ConT name) (ConT schemaName') <- ty
        , name == ''Object
        -> reifySchemaName schemaName'

      _ -> fail $ "'" ++ show schemaName ++ "' is not a Schema"

    parseSchema :: TypeWithoutKinds -> Q SchemaV
    parseSchema ty = maybe (fail $ "Could not parse schema: " ++ show ty) return $ do
      schemaObjectType <- case ty of
        AppT (PromotedT name) schemaType | name == 'Schema -> Just schemaType
        _ -> Nothing

      Schema <$> parseSchemaObjectType schemaObjectType

    parseSchemaObjectType :: TypeWithoutKinds -> Maybe SchemaObjectMapV
    parseSchemaObjectType schemaObjectType = do
      schemaObjectListOfPairs <- mapM typeToPair =<< typeToList schemaObjectType
      forM schemaObjectListOfPairs $ \(schemaKeyType, schemaTypeType) -> do
        schemaKey <- parseSchemaKey schemaKeyType
        schemaType <- parseSchemaType schemaTypeType
        Just (schemaKey, schemaType)

    parseSchemaKey :: TypeWithoutKinds -> Maybe SchemaKeyV
    parseSchemaKey = \case
      AppT (PromotedT ty) (LitT (StrTyLit key))
        | ty == 'NormalKey -> Just $ NormalKey key
        | ty == 'PhantomKey -> Just $ PhantomKey key
      _ -> Nothing

    parseSchemaType :: TypeWithoutKinds -> Maybe SchemaTypeV
    parseSchemaType = \case
      AppT (PromotedT name) (ConT inner)
        | name == 'SchemaScalar -> Just $ SchemaScalar $ nameBase inner

      AppT (PromotedT name) inner
        | name == 'SchemaMaybe  -> SchemaMaybe <$> parseSchemaType inner

        | name == 'SchemaTry    -> SchemaTry <$> parseSchemaType inner

        | name == 'SchemaList   -> SchemaList <$> parseSchemaType inner

        | name == 'SchemaUnion  -> do
            schemas <- typeToList inner
            SchemaUnion <$> mapM parseSchemaType schemas

        | name == 'SchemaObject -> SchemaObject <$> parseSchemaObjectType inner

      _ -> Nothing

typeToList :: TypeWithoutKinds -> Maybe [TypeWithoutKinds]
typeToList = \case
  PromotedNilT -> Just []
  AppT (AppT PromotedConsT x) xs -> (x:) <$> typeToList xs
  _ -> Nothing

typeToPair :: TypeWithoutKinds -> Maybe (TypeWithoutKinds, TypeWithoutKinds)
typeToPair = \case
  AppT (AppT (PromotedTupleT 2) a) b -> Just (a, b)
  _ -> Nothing

{- LookupMap utilities -}

type LookupMap k v = [(k, v)]

-- | Distribute the given element across the values in the map.
distribute :: LookupMap k v -> a -> LookupMap k (a, v)
distribute lookupMap a = map (fmap (a,)) lookupMap

-- | Find all values with the same key (according to the given function) and group them.
--
-- Invariants:
-- * [v] has length > 0
-- * If the first occurence of k1 is before the first occurence of k2, k1 is before k2
--   in the result
groupByKeyWith :: (Eq a, Hashable a) => (k -> a) -> LookupMap k v -> LookupMap k [v]
groupByKeyWith f pairs = map (\key -> (key, groups HashMap.! f key)) distinctKeys
  where
    -- don't use sort; keys should stay in the same order
    distinctKeys = nubBy ((==) `on` f) $ map fst pairs

    groups = HashMap.fromListWith (flip (++)) $ map (\(k, v) -> (f k, [v])) pairs
