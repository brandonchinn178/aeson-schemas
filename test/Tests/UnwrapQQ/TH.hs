{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.UnwrapQQ.TH where

import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter(quoteType))
import Language.Haskell.TH.TestUtils (tryQErr')

import Data.Aeson.Schema (schema, unwrap)

type ListSchema = [schema| { ids: List Int } |]
type MaybeSchema = [schema| { class: Maybe Text } |]
type SumSchema = [schema| { verbosity: Int | Bool } |]

type MySchema = [schema|
  {
    users: List {
      name: Text,
    },
  }
|]

getUnwrapQQErr :: String -> ExpQ
getUnwrapQQErr = tryQErr' . quoteType unwrap
