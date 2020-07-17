{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.SchemaQQ.TH where

import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter(quoteType))
import Language.Haskell.TH.TestUtils (tryQErr')

import Data.Aeson.Schema (schema)

type UserSchema = [schema| { name: Text } |]
type ExtraSchema = [schema| { extra: Text } |]
type ExtraSchema2 = [schema| { extra: Maybe Text } |]

getSchemaQQErr :: String -> ExpQ
getSchemaQQErr = tryQErr' . quoteType schema
