module Schemas
  ( genSchema
  , genSchemaDef
  ) where

import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Aeson.Schema (schema)

genSchema :: Int -> TypeQ
genSchema = quoteType schema . genSchemaDef

genSchemaDef :: Int -> String
genSchemaDef n = "{" ++ intercalate "," (map mkField [1..n]) ++ "}"
  where
    mkField i = "a" ++ show i ++ ": Int"

