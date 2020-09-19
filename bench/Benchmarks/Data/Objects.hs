module Benchmarks.Data.Objects where

import Data.Aeson (FromJSON, Value, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import qualified Data.Text as Text

import Benchmarks.Data.Schemas (mkField)

coerceJSON :: FromJSON a => Value -> a
coerceJSON = fromJust . Aeson.decode . Aeson.encode

-- | An Aeson Value with 100 fields set to vInt.
--
-- Matches any of the SchemaN schemas.
v100Ints :: Value
v100Ints = Aeson.object
  [ Text.pack (mkField i) .= vInt
  | i <- [1..100]
  ]

-- | An Aeson Value with N levels of nesting.
--
-- Matches a SchemaNestN schema for the same N.
vNested :: Int -> Value
vNested n = foldr
  (\i inner -> Aeson.object [Text.pack (mkField i) .= inner])
  vInt
  [1..n]

vInt :: Value
vInt = Aeson.Number 0
