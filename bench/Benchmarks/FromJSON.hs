{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Benchmarks.FromJSON where

import Control.DeepSeq (NFData(..))
import Criterion.Main
import qualified Data.Aeson as Aeson

import Data.Aeson.Schema (Object)

import Benchmarks.Data.Objects
import Benchmarks.Data.Schemas

benchmarks :: Benchmark
benchmarks = bgroup "FromJSON instance"
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

{- Orphans -}

instance Show (Object schema) => NFData (Object schema) where
  rnf = rnf . show
