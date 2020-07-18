{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.GetQQ.TH where

import Data.Aeson.QQ (aesonQQ)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(quoteExp))
import Language.Haskell.TH.TestUtils (tryQ, tryQErr')

import Data.Aeson.Schema (Object, get, schema)
import TestUtils (parseValue)

-- For testing namespaced object
testData :: Object [schema| { foo: Int } |]
testData = parseValue [aesonQQ| { "foo": 1 } |]

tryGetQQ :: String -> ExpQ
tryGetQQ = tryQ . quoteExp get

getGetQQErr :: String -> ExpQ
getGetQQErr = tryQErr' . quoteExp get
