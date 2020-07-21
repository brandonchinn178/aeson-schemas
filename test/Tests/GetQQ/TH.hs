{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.GetQQ.TH where

import Data.Aeson.QQ (aesonQQ)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.TestUtils (tryQ)

import Data.Aeson.Schema (Object, get, schema)
import TestUtils (mkExpQQ, parseValue)
import TestUtils.MockQ (emptyMockQ, runMockQ, runMockQErr)

-- For testing namespaced object
testData :: Object [schema| { foo: Int } |]
testData = parseValue [aesonQQ| { "foo": 1 } |]

tryGetQQ :: String -> ExpQ
tryGetQQ = tryQ . quoteExp get

-- | Run the `get` quasiquoter at both runtime and compile-time, to get coverage.
--
-- The `get` Quasiquoter doesn't reify anything, so this should work.
runGet :: QuasiQuoter
runGet = mkExpQQ $ \s -> [| runMockQ emptyMockQ (quoteExp get s) `seq` $(quoteExp get s) |]

getErr :: QuasiQuoter
getErr = mkExpQQ $ \s -> [| runMockQErr emptyMockQ (quoteExp get s) |]
