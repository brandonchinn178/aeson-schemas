{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

import Control.DeepSeq (NFData(..))
import Criterion.Main
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Language.Haskell.TH.Quote (QuasiQuoter(quoteType))
import Language.Haskell.TH.TestUtils
    (QMode(..), QState(..), loadNames, runTestQ)

import qualified Data.Aeson.Schema
import Data.Aeson.Schema.Internal (Object(..))
import DeepSeq ()
import Schemas

main :: IO ()
main = defaultMain
  [ schemaQQBenchmarks
  , showBenchmarks
  , fromJSONBenchmarks
  ]

schemaQQBenchmarks :: Benchmark
schemaQQBenchmarks = bgroup "schema quasiquoter"
  [ byKeys
  , byNestedKeys
  , byNumOfIncluded
  , byKeysInIncluded
  , byNumOfExtended
  , byKeysInExtended
  ]
  where
    byKeys = bgroup "# of keys" $
      flip map [1, 5, 10, 100] $ \n ->
        let schemaDef = genSchemaDef $ keysTo n
        in bench (show n) $ nf runSchema schemaDef

    byNestedKeys = bgroup "# of nested keys" $
      flip map [1, 5, 10, 100] $ \n ->
        let schemaDef = iterateN n (\prev -> genSchemaDef [Field "a" prev]) "Int"
        in bench (show n) $ nf runSchema schemaDef

    byNumOfIncluded = bgroup "Include given # of schemas" $
      flip map [1, 5, 10, 100] $ \n ->
        let schemaDef = genSchemaDef $ map (\name -> Include name name) $ take n singleSchemas
        in bench (show n) $ nf runSchema schemaDef

    byKeysInIncluded = bgroup "Include schema with given # of keys" $
      flip map sizedSchemas $ \(n, schema) ->
        let schemaDef = genSchemaDef [Include "a" schema]
        in bench (show n) $ nf runSchema schemaDef

    byNumOfExtended = bgroup "Extend given # of schemas" $
      flip map [1, 5, 10, 100] $ \n ->
        let schemaDef = genSchemaDef $ map Ref $ take n singleSchemas
        in bench (show n) $ nf runSchema schemaDef

    byKeysInExtended = bgroup "Extend schema with given # of keys" $
      flip map sizedSchemas $ \(n, schema) ->
        let schemaDef = genSchemaDef [Field "a" "Int", Ref schema]
        in bench (show n) $ nf runSchema schemaDef

    runSchema =
      let qstate = QState
            { mode = MockQ
            , knownNames = sizedSchemasNames ++ singleSchemasNames
            , reifyInfo = $(loadNames $ map snd $ sizedSchemasNames ++ singleSchemasNames)
            }
      in runTestQ qstate . quoteType Data.Aeson.Schema.schema

showBenchmarks :: Benchmark
showBenchmarks = bgroup "Show instance"
  [ byKeys
  , byNestedKeys
  ]
  where
    byKeys = bgroup "# of keys"
      [ bench "1" $ nf show $ coerceJSON @(Object Schema1) v100Ints
      , bench "5" $ nf show $ coerceJSON @(Object Schema5) v100Ints
      , bench "10" $ nf show $ coerceJSON @(Object Schema10) v100Ints
      , bench "100" $ nf show $ coerceJSON @(Object Schema100) v100Ints
      ]

    byNestedKeys = bgroup "# of nested keys"
      [ bench "1" $ nf show $ coerceJSON @(Object SchemaNest1) (vNested 1)
      , bench "5" $ nf show $ coerceJSON @(Object SchemaNest5) (vNested 5)
      , bench "10" $ nf show $ coerceJSON @(Object SchemaNest10) (vNested 10)
      , bench "100" $ nf show $ coerceJSON @(Object SchemaNest100) (vNested 100)
      ]

    coerceJSON :: Aeson.FromJSON a => Aeson.Value -> a
    coerceJSON = fromJust . Aeson.decode . Aeson.encode

    v100Ints = Aeson.object
      [ Text.pack (mkField i) .= vInt
      | i <- [1..100]
      ]

    vNested n = foldr
      (\i inner -> Aeson.object [Text.pack (mkField i) .= inner])
      (Aeson.toJSON vInt)
      [1..n]

    vInt = 0 :: Int

fromJSONBenchmarks :: Benchmark
fromJSONBenchmarks = bgroup "FromJSON instance"
  [ byKeys
  , byNestedKeys
  ]
  where
    byKeys = bgroup "# of keys"
      [ bench "1" $ nf (Aeson.fromJSON @(Object Schema1)) v100Ints
      , bench "5" $ nf (Aeson.fromJSON @(Object Schema5)) v100Ints
      , bench "10" $ nf (Aeson.fromJSON @(Object Schema10)) v100Ints
      , bench "100" $ nf (Aeson.fromJSON @(Object Schema100)) v100Ints
      ]

    byNestedKeys = bgroup "# of nested keys"
      [ bench "1" $ nf (Aeson.fromJSON @(Object SchemaNest1)) (vNested 1)
      , bench "5" $ nf (Aeson.fromJSON @(Object SchemaNest5)) (vNested 5)
      , bench "10" $ nf (Aeson.fromJSON @(Object SchemaNest10)) (vNested 10)
      , bench "100" $ nf (Aeson.fromJSON @(Object SchemaNest100)) (vNested 100)
      ]

    v100Ints = Aeson.object
      [ Text.pack (mkField i) .= vInt
      | i <- [1..100]
      ]

    vNested n = foldr
      (\i inner -> Aeson.object [Text.pack (mkField i) .= inner])
      (Aeson.toJSON vInt)
      [1..n]

    vInt = 0 :: Int

{- Orphans -}

instance Show (Object schema) => NFData (Object schema) where
  rnf = rnf . show

{- Utilities -}

-- | Apply the given functions the given number of times.
--
-- The first parameter must be >= 0.
iterateN :: Int -> (a -> a) -> a -> a
iterateN n f x = iterate f x !! n
