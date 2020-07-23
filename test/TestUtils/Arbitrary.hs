{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestUtils.Arbitrary
  ( ArbitraryObject(..)
  , forAllArbitraryObjects
  ) where

import Data.Aeson (FromJSON, ToJSON(..), Value(..))
import qualified Data.Aeson as Aeson
import Data.List (stripPrefix)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Language.Haskell.TH (ExpQ, listE)
import Language.Haskell.TH.Quote (QuasiQuoter(quoteType))
import Language.Haskell.TH.Syntax (Lift)
import Test.QuickCheck

import Data.Aeson.Schema (Object, schema)
import Data.Aeson.Schema.Internal (SchemaType(..))
import qualified Data.Aeson.Schema.Internal as Internal
import qualified Data.Aeson.Schema.Show as SchemaShow
import TestUtils (parseValue)

-- Orphan instances
deriving instance Lift SchemaShow.SchemaKey
deriving instance Lift SchemaShow.SchemaType

data ArbitraryObject where
  ArbitraryObject
    :: ( Eq (Object schema)
       , Show (Object schema)
       , FromJSON (Object schema)
       , ToJSON (Object schema)
       )
    => Object schema
    -> SchemaShow.SchemaType
    -> ArbitraryObject

deriving instance Show ArbitraryObject

-- | A Template Haskell function to generate a splice for QuickCheck tests to generate arbitrary
-- objects with arbitrary schemas.
arbitraryObject :: ExpQ
arbitraryObject = do
  -- TODO: generate
  let arbitrarySchemas =
        [ SchemaShow.SchemaObject [(SchemaShow.NormalKey "foo", SchemaShow.SchemaScalar "Bool")]
        , SchemaShow.SchemaObject [(SchemaShow.NormalKey "foo", SchemaShow.SchemaScalar "Int")]
        , SchemaShow.SchemaObject [(SchemaShow.NormalKey "foo", SchemaShow.SchemaScalar "Double")]
        , SchemaShow.SchemaObject [(SchemaShow.NormalKey "foo", SchemaShow.SchemaScalar "Text")]
        ]

  [| oneof $(listE $ map mkSchemaGen arbitrarySchemas) |]
  where
    mkSchemaGen schemaShow =
      let schemaTypeShow = SchemaShow.showSchemaType schemaShow
          schemaType = case "SchemaObject " `stripPrefix` schemaTypeShow of
            Just s -> quoteType schema s
            Nothing -> error $ "Invalid schema: " ++ schemaTypeShow
      in [| genSchema' (Proxy :: Proxy $schemaType) schemaShow |]

-- | Splices to a 'forAll' with 'arbitraryObject', outputting information about the object
-- generated, to ensure we get good generation.
--
-- $(forAllArbitraryObjects) :: Testable prop => ArbitraryObject -> prop
forAllArbitraryObjects :: ExpQ
forAllArbitraryObjects = [| \runTest ->
  forAll $arbitraryObject $ \o@(ArbitraryObject _ schemaType) ->
    tabulate' "Key types" (map getKeyType $ getKeys schemaType) $
    tabulate' "Schema types" (getSchemaTypes schemaType) $
    runTest o
  |]

genSchema' :: forall schema inner.
  ( schema ~ 'Internal.Schema inner
  , ArbitrarySchema ('SchemaObject inner)
  , Internal.IsSchemaType ('SchemaObject inner)
  )
  => Proxy schema -> SchemaShow.SchemaType -> Gen ArbitraryObject
genSchema' _ schemaType = do
  v <- genSchema @('SchemaObject inner)

  let o = parseValue @(Object schema) v

  return $ ArbitraryObject o schemaType

getKeyType :: SchemaShow.SchemaKey -> String
getKeyType = \case
  SchemaShow.NormalKey _ -> "Normal"
  SchemaShow.PhantomKey _ -> "Phantom"

getKeys :: SchemaShow.SchemaType -> [SchemaShow.SchemaKey]
getKeys = \case
  SchemaShow.SchemaMaybe inner -> getKeys inner
  SchemaShow.SchemaTry inner -> getKeys inner
  SchemaShow.SchemaList inner -> getKeys inner
  SchemaShow.SchemaUnion schemas -> concatMap getKeys schemas
  SchemaShow.SchemaObject pairs -> concatMap (\(key, inner) -> key : getKeys inner) pairs
  _ -> []

getSchemaTypes :: SchemaShow.SchemaType -> [String]
getSchemaTypes = \case
  SchemaShow.SchemaScalar s -> [s]
  SchemaShow.SchemaMaybe inner -> "SchemaMaybe" : getSchemaTypes inner
  SchemaShow.SchemaTry inner -> "SchemaTry" : getSchemaTypes inner
  SchemaShow.SchemaList inner -> "SchemaList" : getSchemaTypes inner
  SchemaShow.SchemaUnion schemas -> "SchemaUnion" : concatMap getSchemaTypes schemas
  SchemaShow.SchemaObject pairs -> "SchemaObject" : concatMap (getSchemaTypes . snd) pairs

tabulate' :: String -> [String] -> Property -> Property
#if MIN_VERSION_QuickCheck(2,12,0)
tabulate' = tabulate
#else
tabulate' _ _ = id
#endif

{- Generating schemas -}

class ArbitrarySchema (schema :: SchemaType) where
  genSchema :: Gen Value

instance {-# OVERLAPS #-} ArbitrarySchema ('SchemaScalar Text) where
  genSchema = toJSON <$> arbitrary @String

instance (Arbitrary inner, ToJSON inner, Typeable inner) => ArbitrarySchema ('SchemaScalar inner) where
  genSchema = toJSON <$> arbitrary @inner

instance ArbitraryObjectMap schema => ArbitrarySchema ('SchemaObject schema) where
  genSchema = Object <$> genSchemaMap @schema

class ArbitraryObjectMap (a :: Internal.SchemaObjectMap) where
  genSchemaMap :: Gen Aeson.Object

instance ArbitraryObjectMap '[] where
  genSchemaMap = return mempty

instance
  ( Internal.IsSchemaKey key
  , ArbitrarySchema inner
  , ArbitraryObjectMap rest
  ) => ArbitraryObjectMap ( '(key, inner) ': rest ) where

  genSchemaMap = do
    inner <- genSchema @inner
    rest <- genSchemaMap @rest
    return $ Internal.buildContext @key inner rest
