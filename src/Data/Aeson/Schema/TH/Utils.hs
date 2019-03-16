{-|
Module      :  Data.Aeson.Schema.TH.Utils
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aeson.Schema.TH.Utils where

import Data.Bifunctor (second)
import Language.Haskell.TH

import Data.Aeson.Schema.Internal (SchemaType(..))
import qualified Data.Aeson.Schema.Show as SchemaShow

-- | Show the given schema as a type.
showSchemaType :: Type -> String
showSchemaType = SchemaShow.showSchemaType . fromSchemaType
  where
    fromSchemaType = \case
      PromotedT name
        | name == 'SchemaBool -> SchemaShow.SchemaBool
        | name == 'SchemaInt -> SchemaShow.SchemaInt
        | name == 'SchemaDouble -> SchemaShow.SchemaDouble
        | name == 'SchemaText -> SchemaShow.SchemaText
      AppT (PromotedT name) (ConT inner)
        | name == 'SchemaCustom -> SchemaShow.SchemaCustom $ nameBase inner
      AppT (PromotedT name) inner
        | name == 'SchemaMaybe -> SchemaShow.SchemaMaybe $ fromSchemaType inner
        | name == 'SchemaList -> SchemaShow.SchemaList $ fromSchemaType inner
        | name == 'SchemaObject -> SchemaShow.SchemaObject $ fromPairs inner
      ty -> error $ "Unknown type: " ++ show ty
    fromPairs pairs = map (second fromSchemaType) $ fromTypeList' pairs

fromTypeList' :: Type -> [(String, Type)]
fromTypeList' = \case
  PromotedNilT -> []
  AppT (AppT PromotedConsT x) xs ->
    case x of
      AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit k))) v ->
        let pair = (k, stripSigs v)
        in pair : fromTypeList' xs
      _ -> error $ "Not a type-level tuple: " ++ show x
  SigT ty _ -> fromTypeList' ty
  ty -> error $ "Not a type-level list: " ++ show ty

fromTypeList :: Type -> Q [(String, TypeQ)]
fromTypeList = pure . map (second pure) . fromTypeList'

toTypeList :: [(String, TypeQ)] -> TypeQ
toTypeList = foldr (consT . pairT) promotedNilT
  where
    pairT (k, v) = [t| '( $(litT $ strTyLit k), $v) |]
    consT x xs = [t| $x ': $xs |]

-- | Strip all kind signatures from the given type.
stripSigs :: Type -> Type
stripSigs = \case
  ForallT tyVars ctx ty -> ForallT tyVars ctx (stripSigs ty)
  AppT ty1 ty2 -> AppT (stripSigs ty1) (stripSigs ty2)
  SigT ty _ -> stripSigs ty
  InfixT ty1 name ty2 -> InfixT (stripSigs ty1) name (stripSigs ty2)
  UInfixT ty1 name ty2 -> UInfixT (stripSigs ty1) name (stripSigs ty2)
  ParensT ty -> ParensT (stripSigs ty)
  ty -> ty
