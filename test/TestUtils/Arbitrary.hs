{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
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

module TestUtils.Arbitrary
  ( ArbitraryObject(..)
  , forAllArbitraryObjects
  ) where

import Data.Aeson (FromJSON, ToJSON(..), Value(..))
import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Haskell.TH (ExpQ, listE)
import Language.Haskell.TH.Quote (QuasiQuoter(quoteType))
import Test.QuickCheck

import Data.Aeson.Schema (Object, schema)
import Data.Aeson.Schema.Internal (SchemaKey(..), SchemaType(..))
import qualified Data.Aeson.Schema.Internal as Internal
import qualified Data.Aeson.Schema.Show as SchemaShow
import TestUtils (parseValue)

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
        [ "{ foo: Bool }"
        , "{ foo: Int }"
        , "{ foo: Double }"
        , "{ foo: Text }"
        ]

  [| oneof $(listE $ map mkSchemaGen arbitrarySchemas) |]
  where
    mkSchemaGen s = [| genSchema' (Proxy :: Proxy $(quoteType schema s)) |]

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
  => Proxy schema -> Gen ArbitraryObject
genSchema' _ = do
  (v, schemaType) <- genSchema @('SchemaObject inner)
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
  SchemaShow.SchemaBool -> ["SchemaBool"]
  SchemaShow.SchemaInt -> ["SchemaInt"]
  SchemaShow.SchemaDouble -> ["SchemaDouble"]
  SchemaShow.SchemaText -> ["SchemaText"]
  SchemaShow.SchemaCustom _ -> ["SchemaCustom"]
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
  genSchema :: Gen (Value, SchemaShow.SchemaType)

instance ArbitrarySchema 'SchemaBool where
  genSchema = (, SchemaShow.SchemaBool) . toJSON <$> arbitrary @Bool

instance ArbitrarySchema 'SchemaInt where
  genSchema = (, SchemaShow.SchemaInt) . toJSON <$> arbitrary @Int

instance ArbitrarySchema 'SchemaDouble where
  genSchema = (, SchemaShow.SchemaDouble) . toJSON <$> arbitrary @Double

instance ArbitrarySchema 'SchemaText where
  genSchema = (, SchemaShow.SchemaText) . toJSON <$> arbitrary @String

instance ArbitraryObjectMap schema => ArbitrarySchema ('SchemaObject schema) where
  genSchema = do
    (o, schemaType) <- genSchemaMap @schema
    return (Object o, SchemaShow.SchemaObject schemaType)

class ArbitraryObjectMap (a :: Internal.SchemaObjectMap) where
  genSchemaMap :: Gen (Aeson.Object, [(SchemaShow.SchemaKey, SchemaShow.SchemaType)])

instance ArbitraryObjectMap '[] where
  genSchemaMap = return (mempty, [])

instance
  ( Internal.IsSchemaKey key
  , SchemaKeyShow key
  , ArbitrarySchema inner
  , ArbitraryObjectMap rest
  ) => ArbitraryObjectMap ( '(key, inner) ': rest ) where

  genSchemaMap = do
    (inner, innerSchema) <- genSchema @inner
    (rest, restSchema) <- genSchemaMap @rest
    return (Internal.buildContext @key inner rest, (toSchemaKeyShow @key, innerSchema) : restSchema)

class SchemaKeyShow (key :: SchemaKey) where
  toSchemaKeyShow :: SchemaShow.SchemaKey

instance KnownSymbol key => SchemaKeyShow ('NormalKey key) where
  toSchemaKeyShow = SchemaShow.NormalKey $ symbolVal $ Proxy @key

instance KnownSymbol key => SchemaKeyShow ('PhantomKey key) where
  toSchemaKeyShow = SchemaShow.PhantomKey $ symbolVal $ Proxy @key
