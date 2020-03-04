{-# LANGUAGE DataKinds #-}

module SumType where

import Data.Aeson.Schema.Utils.Sum (SumType(..))

type SpecialJSON = SumType '[Bool, Int, [String]]
