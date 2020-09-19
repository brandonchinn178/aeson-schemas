{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Benchmarks.Show where

import Criterion.Main

import Data.Aeson.Schema (Object)

import Benchmarks.Data.Objects
import Benchmarks.Data.Schemas

benchmarks :: Benchmark
benchmarks = bgroup "Show instance"
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
