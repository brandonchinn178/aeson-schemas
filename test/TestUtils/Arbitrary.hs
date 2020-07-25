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

import Control.Monad (forM)
import Data.Aeson (FromJSON, ToJSON(..), Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.List (nub, stripPrefix)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.Exts (fromList)
import Language.Haskell.TH (ExpQ, listE, runIO)
import Language.Haskell.TH.Quote (QuasiQuoter(quoteType))
import Language.Haskell.TH.Syntax (Lift)
import Test.QuickCheck

import Data.Aeson.Schema (Object, schema)
import Data.Aeson.Schema.Internal (SchemaType(..))
import qualified Data.Aeson.Schema.Internal as Internal
import qualified Data.Aeson.Schema.Show as SchemaShow

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
--
-- Note that for repeated runs of the test suite, the schemas will be the same, with the actual
-- JSON values generated randomly. You need to recompile in order to generate different schemas.
arbitraryObject :: ExpQ
arbitraryObject = do
  arbitrarySchemas <- runIO $ genSchemaTypes 5

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
    tabulate' "Object sizes" (map show $ getObjectSizes schemaType) $
    tabulate' "Object depth" [show $ getObjectDepth schemaType] $
    runTest o
  |]

{- Run time helpers -}

genSchema' :: forall schema inner.
  ( schema ~ 'Internal.Schema inner
  , ArbitrarySchema ('SchemaObject inner)
  , Internal.IsSchemaType ('SchemaObject inner)
  )
  => Proxy schema -> SchemaShow.SchemaType -> Gen ArbitraryObject
genSchema' _ schemaType = do
  v <- genSchema @('SchemaObject inner)

  case Aeson.parseEither Aeson.parseJSON v of
    Right (o :: Object schema) -> return $ ArbitraryObject o schemaType
    Left e -> error $ unlines
      [ "Could not parse " ++ SchemaShow.showSchemaType schemaType
      , show v
      , e
      ]

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

getObjectSizes :: SchemaShow.SchemaType -> [Int]
getObjectSizes = \case
  SchemaShow.SchemaScalar _ -> []
  SchemaShow.SchemaMaybe inner -> getObjectSizes inner
  SchemaShow.SchemaTry inner -> getObjectSizes inner
  SchemaShow.SchemaList inner -> getObjectSizes inner
  SchemaShow.SchemaUnion schemas -> concatMap getObjectSizes schemas
  SchemaShow.SchemaObject pairs -> length pairs : concatMap (getObjectSizes . snd) pairs

getObjectDepth :: SchemaShow.SchemaType -> Int
getObjectDepth = \case
  SchemaShow.SchemaScalar _ -> 0
  SchemaShow.SchemaMaybe inner -> getObjectDepth inner
  SchemaShow.SchemaTry inner -> getObjectDepth inner
  SchemaShow.SchemaList inner -> getObjectDepth inner
  SchemaShow.SchemaUnion schemas -> maximum $ map getObjectDepth schemas
  SchemaShow.SchemaObject pairs -> 1 + maximum (map (getObjectDepth . snd) pairs)

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

instance ArbitrarySchema inner => ArbitrarySchema ('SchemaMaybe inner) where
  genSchema = frequency
    [ (3, genSchema @inner)
    , (1, pure Null)
    ]

instance ArbitrarySchema inner => ArbitrarySchema ('SchemaTry inner) where
  genSchema = frequency
    [ (3, genSchema @inner)
    , (1, genValue)
    ]
    where
      genValue = oneof
        [ pure Null
        , Number . realToFrac <$> arbitrary @Double
        , Bool <$> arbitrary
        , String . Text.pack <$> arbitrary
        ]

instance ArbitrarySchema inner => ArbitrarySchema ('SchemaList inner) where
  genSchema = Array . fromList <$> listOf (genSchema @inner)

instance (ArbitrarySchemaList schemas) => ArbitrarySchema ('SchemaUnion schemas) where
  genSchema = oneof $ genSchemaElems @schemas

class ArbitrarySchemaList xs where
  genSchemaElems :: [Gen Value]

instance ArbitrarySchemaList '[] where
  genSchemaElems = []

instance (ArbitrarySchema x, ArbitrarySchemaList xs) => ArbitrarySchemaList (x ': xs) where
  genSchemaElems = genSchema @x : genSchemaElems @xs

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

{- Generating schema definitions -}

genSchemaTypes :: Int -> IO [SchemaShow.SchemaType]
genSchemaTypes numSchemasToGenerate =
  generate $ sequence $ take numSchemasToGenerate
    [ resize n arbitrary | n <- [0,2..] ]

instance Arbitrary SchemaShow.SchemaType where
  arbitrary = genSchemaObject 5

genSchemaObject :: Int -> Gen SchemaShow.SchemaType
genSchemaObject maxDepth = do
  keys <- genUniqList1 genKey
  pairs <- forM keys $ \key -> frequency
    [ (10, genSchemaObjectPairNormal key)
    , (1, genSchemaObjectPairPhantom key)
    ]

  return $ SchemaShow.SchemaObject pairs
  where
    genSchemaObject' = genSchemaObject $ maxDepth - 1

    genSchemaObjectPairNormal key = do
      schemaType <- frequency $ if maxDepth == 0
        then scalarSchemaTypes
        else allSchemaTypes
      return (SchemaShow.NormalKey key, schemaType)

    genSchemaObjectPairPhantom key = do
      schemaType <- frequency
        [ (2, SchemaShow.SchemaMaybe <$> genSchemaObject')
        , (2, SchemaShow.SchemaTry <$> frequency allSchemaTypes)
        , (4, genSchemaObject')
        , (1, SchemaShow.SchemaUnion <$> scaleHalf (genUniqList1 genSchemaObject'))
        ]
      return (SchemaShow.PhantomKey key, schemaType)

    scalarSchemaTypes =
      [ (4, pure $ SchemaShow.SchemaScalar "Bool")
      , (4, pure $ SchemaShow.SchemaScalar "Int")
      , (4, pure $ SchemaShow.SchemaScalar "Double")
      , (4, pure $ SchemaShow.SchemaScalar "Text")
      ]

    nonNullableSchemaTypes =
      scalarSchemaTypes ++
      [ (2, SchemaShow.SchemaList <$> frequency allSchemaTypes)
      , (1, SchemaShow.SchemaUnion <$> scaleHalf (genUniqList1 $ frequency allSchemaTypes))
      , (2, genSchemaObject')
      ]

    allSchemaTypes =
      nonNullableSchemaTypes ++
      [ (2, SchemaShow.SchemaMaybe <$> frequency nonNullableSchemaTypes)
      , (2, SchemaShow.SchemaTry <$> frequency nonNullableSchemaTypes)
      ]

-- | Generate a valid JSON key
-- See Data.Aeson.Schema.TH.Parse.jsonKey
genKey :: Gen String
genKey = listOf1 $ arbitrary `suchThat` (`notElem` " !?[](),.@:{}#")

-- | Generate a non-empty and unique list of the given generator.
genUniqList1 :: Eq a => Gen a -> Gen [a]
genUniqList1 gen = sized $ \n -> do
  k <- choose (1, max 1 n)
  take k . nub <$> infiniteListOf gen

-- | Scale the generator size by half
scaleHalf :: Gen a -> Gen a
scaleHalf = scale (`div` 2)
