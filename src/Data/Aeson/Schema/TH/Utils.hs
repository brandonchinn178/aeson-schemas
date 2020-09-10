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
{-# LANGUAGE ViewPatterns #-}

module Data.Aeson.Schema.TH.Utils
  ( reifySchema
  , reifySchemaName
  , schemaVToTypeQ
  , schemaTypeVToTypeQ
  ) where

import Control.Monad (forM, (>=>))
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Language.Haskell.TH

import Data.Aeson.Schema.Internal (Object)
import Data.Aeson.Schema.Key (SchemaKey'(..), SchemaKeyV)
import Data.Aeson.Schema.Type
    ( NameLike(..)
    , Schema'(..)
    , SchemaObjectMapV
    , SchemaType'(..)
    , SchemaTypeV
    , SchemaV
    , fromSchemaV
    )

reifySchema :: String -> Q SchemaV
reifySchema name = lookupTypeName name >>= maybe unknownSchemaErr reifySchemaName
  where
    unknownSchemaErr = fail $ "Unknown schema: " ++ name

reifySchemaName :: Name -> Q SchemaV
reifySchemaName = reifySchemaType >=> parseSchema
  where
    reifySchemaType :: Name -> Q TypeWithoutKinds
    reifySchemaType schemaName = reify schemaName >>= \case
      -- reify `type MySchema = 'Schema '[ ... ]`
      TyConI (TySynD _ _ (stripKinds -> ty))
        | AppT (PromotedT name) _ <- ty
        , name == 'Schema
        -> return ty

      -- reify `type MySchema = Object OtherSchema`
      TyConI (TySynD _ _ (stripKinds -> ty))
        | AppT (ConT name) (ConT schemaName') <- ty
        , name == ''Object
        -> reifySchemaType schemaName'

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
        | name == 'SchemaScalar -> Just $ SchemaScalar $ NameTH inner

      AppT (PromotedT name) inner
        | name == 'SchemaMaybe  -> SchemaMaybe <$> parseSchemaType inner

        | name == 'SchemaTry    -> SchemaTry <$> parseSchemaType inner

        | name == 'SchemaList   -> SchemaList <$> parseSchemaType inner

        | name == 'SchemaUnion  -> do
            schemas <- typeToList inner
            SchemaUnion <$> mapM parseSchemaType schemas

        | name == 'SchemaObject -> SchemaObject <$> parseSchemaObjectType inner

      _ -> Nothing

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
  SchemaScalar name     -> [t| 'SchemaScalar $(resolveName name >>= conT) |]
  SchemaMaybe inner     -> [t| 'SchemaMaybe $(schemaTypeVToTypeQ inner) |]
  SchemaTry inner       -> [t| 'SchemaTry $(schemaTypeVToTypeQ inner) |]
  SchemaList inner      -> [t| 'SchemaList $(schemaTypeVToTypeQ inner) |]
  SchemaUnion schemas   -> [t| 'SchemaUnion $(promotedListT $ map schemaTypeVToTypeQ schemas) |]
  SchemaObject pairs    -> [t| 'SchemaObject $(schemaObjectMapVToTypeQ pairs) |]

resolveName :: NameLike -> Q Name
resolveName = \case
  -- some hardcoded cases
  NameRef "Bool"   -> pure ''Bool
  NameRef "Int"    -> pure ''Int
  NameRef "Double" -> pure ''Double
  NameRef "Text"   -> pure ''Text

  NameRef name     -> getType name
  NameTH name      -> pure name
  where
    getType :: String -> Q Name
    getType ty = maybe (fail $ "Unknown type: " ++ ty) pure =<< lookupTypeName ty

{- TH utilities -}

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

typeToList :: TypeWithoutKinds -> Maybe [TypeWithoutKinds]
typeToList = \case
  PromotedNilT -> Just []
  AppT (AppT PromotedConsT x) xs -> (x:) <$> typeToList xs
  _ -> Nothing

typeToPair :: TypeWithoutKinds -> Maybe (TypeWithoutKinds, TypeWithoutKinds)
typeToPair = \case
  AppT (AppT (PromotedTupleT 2) a) b -> Just (a, b)
  _ -> Nothing

promotedListT :: [TypeQ] -> TypeQ
promotedListT = foldr consT promotedNilT
  where
    -- nb. https://stackoverflow.com/a/34457936
    consT x xs = appT (appT promotedConsT x) xs

promotedPairT :: (TypeQ, TypeQ) -> TypeQ
promotedPairT (a, b) = [t| '( $a, $b ) |]
