{-|
Module      :  Data.Aeson.Schema.TH.Utils
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aeson.Schema.TH.Utils where

import Control.Monad ((>=>))
import Data.Bifunctor (bimap, first, second)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH

import Data.Aeson.Schema.Internal (Object, SchemaResult)
import Data.Aeson.Schema.Key (SchemaKey'(..), SchemaKeyV, fromSchemaKeyV)
import Data.Aeson.Schema.TH.Parse (GetterOperation(..), GetterOps)
import Data.Aeson.Schema.Type
    (Schema'(..), SchemaType'(..), SchemaTypeV, showSchemaTypeV)

parseSchemaType :: HasCallStack => Type -> SchemaTypeV
parseSchemaType = \case
  AppT (PromotedT name) (ConT inner)
    | name == 'SchemaScalar -> SchemaScalar $ nameBase inner
  AppT (PromotedT name) inner
    | name == 'SchemaMaybe -> SchemaMaybe $ parseSchemaType inner
    | name == 'SchemaTry -> SchemaTry $ parseSchemaType inner
    | name == 'SchemaList -> SchemaList $ parseSchemaType inner
    | name == 'SchemaUnion -> SchemaUnion $ map parseSchemaType $ typeToList inner
    | name == 'SchemaObject -> SchemaObject $ fromPairs inner
  ty -> error $ "Unknown type: " ++ show ty
  where
    fromPairs pairs = map (second parseSchemaType) $ typeToSchemaPairs pairs

typeToList :: HasCallStack => Type -> [Type]
typeToList = \case
  PromotedNilT -> []
  AppT (AppT PromotedConsT x) xs -> x : typeToList xs
  SigT ty _ -> typeToList ty
  ty -> error $ "Not a type-level list: " ++ show ty

typeToPair :: HasCallStack => Type -> (Type, Type)
typeToPair = \case
  AppT (AppT (PromotedTupleT 2) a) b -> (a, b)
  SigT ty _ -> typeToPair ty
  ty -> error $ "Not a type-level pair: " ++ show ty

typeToSchemaPairs :: HasCallStack => Type -> [(SchemaKeyV, Type)]
typeToSchemaPairs = map (bimap parseSchemaKey stripKinds . typeToPair) . typeToList

parseSchemaKey :: HasCallStack => Type -> SchemaKeyV
parseSchemaKey = \case
  AppT (PromotedT ty) (LitT (StrTyLit key))
    | ty == 'NormalKey -> NormalKey key
    | ty == 'PhantomKey -> PhantomKey key
  SigT ty _ -> parseSchemaKey ty
  ty -> error $ "Could not parse a schema key: " ++ show ty

-- | Same as 'Type' except without any kind signatures or applications at any depth.
--
-- Provides no actual guarantees. The caller is responsible for making sure the value
-- has been run through 'stripKinds' at one point.
type TypeWithoutKinds = Type

-- | Recursively strip all kind signatures and applications.
stripKinds :: Type -> TypeWithoutKinds
stripKinds ty =
  case ty of
    -- cases that strip + recurse
    SigT ty1 _ -> stripKinds ty1
#if MIN_VERSION_template_haskell(2,15,0)
    AppKindT ty1 _ -> stripKinds ty1
#endif

    -- cases that recurse
    ForallT tyVars ctx ty1 -> ForallT tyVars ctx (stripKinds ty1)
#if MIN_VERSION_template_haskell(2,16,0)
    ForallVisT tyVars ty1 -> ForallVisT tyVars (stripKinds ty1)
#endif
    AppT ty1 ty2 -> AppT (stripKinds ty1) (stripKinds ty2)
    InfixT ty1 name ty2 -> InfixT (stripKinds ty1) name (stripKinds ty2)
    UInfixT ty1 name ty2 -> UInfixT (stripKinds ty1) name (stripKinds ty2)
    ParensT ty1 -> ParensT (stripKinds ty1)
#if MIN_VERSION_template_haskell(2,15,0)
    ImplicitParamT str ty1 -> ImplicitParamT str (stripKinds ty1)
#endif

    -- base cases
    VarT _           -> ty
    ConT _           -> ty
    PromotedT _      -> ty
    TupleT _         -> ty
    UnboxedTupleT _  -> ty
    UnboxedSumT _    -> ty
    ArrowT           -> ty
    EqualityT        -> ty
    ListT            -> ty
    PromotedTupleT _ -> ty
    PromotedNilT     -> ty
    PromotedConsT    -> ty
    StarT            -> ty
    ConstraintT      -> ty
    LitT _           -> ty
    WildCardT        -> ty

-- | Reify the given name and return the result if it reifies to a Schema.
reifySchema :: Name -> TypeQ
reifySchema = reify >=> \case
  TyConI (TySynD _ _ tyWithSigs)
    | ty <- stripKinds tyWithSigs
    , AppT (PromotedT name) _ <- ty
    , name == 'Schema
    -> pure ty

  -- also reify (Object schema)
  TyConI (TySynD _ _ tyWithSigs)
    | ty <- stripKinds tyWithSigs
    , AppT (ConT name) (ConT schema) <- ty
    , name == ''Object
    -> reifySchema schema

  info -> fail $ "Unknown reified schema: " ++ show info

-- | Unwrap the given type using the given getter operations.
--
-- Accepts Bool for whether to maintain functor structure (True) or strip away functor applications
-- (False).
unwrapType :: Bool -> GetterOps -> Type -> TypeQ
unwrapType keepFunctor getterOps schemaType =
  case schemaType of
    AppT (PromotedT ty) inner | ty == 'Schema -> go (AppT (PromotedT 'SchemaObject) inner) getterOps
    ty -> fail $ "Tried to unwrap something that wasn't a Schema: " ++ show ty
  where
    go schema [] = [t| SchemaResult $(pure schema) |]
    go schema (op:ops) = case schema of
      AppT (PromotedT ty) inner ->
        case op of
          GetterKey key | ty == 'SchemaObject ->
            let getObjectSchema = map (first getSchemaKey . typeToPair) . typeToList
                getSchemaKey = fromSchemaKeyV . parseSchemaKey
            in case lookup key (getObjectSchema inner) of
              Just nextSchema -> go nextSchema ops
              Nothing -> fail $ "Key '" ++ key ++ "' does not exist in schema: " ++ showSchemaType schema
          GetterKey key -> fail $ "Cannot get key '" ++ key ++ "' in schema: " ++ showSchemaType schema
          GetterList elems | ty == 'SchemaObject -> do
            elemSchemas <- mapM (go schema) elems
            if all (== head elemSchemas) elemSchemas
              then appT listT (pure $ head elemSchemas)
              else fail $ "List contains different types with schema: " ++ showSchemaType schema
          GetterList _ -> fail $ "Cannot get keys in schema: " ++ showSchemaType schema
          GetterTuple elems | ty == 'SchemaObject ->
            foldl appT (tupleT $ length elems) $ map (go schema) elems
          GetterTuple _ -> fail $ "Cannot get keys in schema: " ++ showSchemaType schema
          GetterBang | ty == 'SchemaMaybe -> go inner ops
          GetterBang | ty == 'SchemaTry -> go inner ops
          GetterBang -> fail $ "Cannot use `!` operator on schema: " ++ showSchemaType schema
          GetterMapMaybe | ty == 'SchemaMaybe -> withFunctor [t| Maybe |] $ go inner ops
          GetterMapMaybe | ty == 'SchemaTry -> withFunctor [t| Maybe |] $ go inner ops
          GetterMapMaybe -> fail $ "Cannot use `?` operator on schema: " ++ showSchemaType schema
          GetterMapList | ty == 'SchemaList -> withFunctor (pure ListT) $ go inner ops
          GetterMapList -> fail $ "Cannot use `[]` operator on schema: " ++ showSchemaType schema
          GetterBranch branch | ty == 'SchemaUnion ->
            let subTypes = typeToList inner
            in if branch >= length subTypes
              then fail $ "Branch out of bounds for schema: " ++ showSchemaType schema
              else go (subTypes !! branch) ops
          GetterBranch _ -> fail $ "Cannot use `@` operator on schema: " ++ showSchemaType schema

      _ -> fail $ unlines ["Cannot get type:", show schema, show op]

    withFunctor f = if keepFunctor then appT f else id

    showSchemaType = showSchemaTypeV . parseSchemaType
