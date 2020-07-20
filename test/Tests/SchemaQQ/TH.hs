{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.SchemaQQ.TH where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.TestUtils (tryQErr')

import Data.Aeson.Schema (schema)

type UserSchema = [schema| { name: Text } |]
type ExtraSchema = [schema| { extra: Text } |]
type ExtraSchema2 = [schema| { extra: Maybe Text } |]

schemaErr :: QuasiQuoter
schemaErr = QuasiQuoter
  { quoteExp = \s -> [| $(tryQErr' $ quoteType schema s) :: String |]
  , quoteDec = error "Cannot use `schemaErr` for Dec"
  , quoteType = error "Cannot use `schemaErr` for Type"
  , quotePat = error "Cannot use `schemaErr` for Pat"
  }
