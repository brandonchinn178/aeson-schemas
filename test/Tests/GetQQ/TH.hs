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
import TestUtils.MockQ (emptyMockQ, runMockQ)

-- For testing namespaced object
testData :: Object [schema| { foo: Int } |]
testData = parseValue [aesonQQ| { "foo": 1 } |]

tryGetQQ :: String -> ExpQ
tryGetQQ = tryQ . quoteExp get

-- | Run the `get` quasiquoter at both runtime and compile-time, to get coverage.
--
-- The `get` Quasiquoter doesn't reify anything, so this should work.
runGet :: QuasiQuoter
runGet = QuasiQuoter
  { quoteExp = \s -> [| runMockQ emptyMockQ (quoteExp get s) `seq` $(quoteExp get s) |]
  , quoteDec = error "Cannot use `runGet` for Dec"
  , quoteType = error "Cannot use `runGet` for Type"
  , quotePat = error "Cannot use `runGet` for Pat"
  }

getErr :: QuasiQuoter
getErr = QuasiQuoter
  { quoteExp = \s -> [| $(tryQErr' $ quoteExp get s) :: String |]
  , quoteDec = error "Cannot use `getErr` for Dec"
  , quoteType = error "Cannot use `getErr` for Type"
  , quotePat = error "Cannot use `getErr` for Pat"
  }
