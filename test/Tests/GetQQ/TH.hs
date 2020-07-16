{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.GetQQ.TH where

import Data.Aeson (FromJSON, Value, parseJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (parseEither)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(quoteExp, quoteType))
import Language.Haskell.TH.TestUtils (tryQ, tryQErr')

import Data.Aeson.Schema (Object, get, schema)

parseValue :: FromJSON a => Value -> a
parseValue = either error id . parseEither parseJSON

parseObject :: String -> ExpQ
parseObject schemaString = [| parseValue :: Value -> Object $schemaType |]
  where
    schemaType = quoteType schema schemaString

-- For testing namespaced object
testData :: Object [schema| { foo: Int } |]
testData = parseValue [aesonQQ| { "foo": 1 } |]

tryGetQQ :: String -> ExpQ
tryGetQQ = tryQ . quoteExp get

getGetQQErr :: String -> ExpQ
getGetQQErr = tryQErr' . quoteExp get
