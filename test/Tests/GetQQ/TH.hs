{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.GetQQ.TH where

import Data.Aeson.QQ (aesonQQ)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.TestUtils (tryQ, tryQErr')

import Data.Aeson.Schema (Object, get, schema)
import TestUtils (parseValue)

-- For testing namespaced object
testData :: Object [schema| { foo: Int } |]
testData = parseValue [aesonQQ| { "foo": 1 } |]

tryGetQQ :: String -> ExpQ
tryGetQQ = tryQ . quoteExp get

getErr :: QuasiQuoter
getErr = QuasiQuoter
  { quoteExp = \s -> [| $(tryQErr' $ quoteExp get s) :: String |]
  , quoteDec = error "Cannot use `getErr` for Dec"
  , quoteType = error "Cannot use `getErr` for Type"
  , quotePat = error "Cannot use `getErr` for Pat"
  }
