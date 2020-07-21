{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.GetQQ.TH where

import Control.DeepSeq (deepseq)
import Data.Aeson.QQ (aesonQQ)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema (Object, get, schema)
import TestUtils (mkExpQQ, parseValue)
import TestUtils.DeepSeq ()
import TestUtils.MockQ (emptyMockQ, runMockQ, runMockQErr)

-- For testing namespaced object
testData :: Object [schema| { foo: Maybe Int } |]
testData = parseValue [aesonQQ| { "foo": null } |]

-- | Run the `get` quasiquoter at both runtime and compile-time, to get coverage.
--
-- The `get` Quasiquoter doesn't reify anything, so this should work.
runGet :: QuasiQuoter
runGet = mkExpQQ $ \s -> [| runMockQ emptyMockQ (quoteExp get s) `deepseq` $(quoteExp get s) |]

getErr :: QuasiQuoter
getErr = mkExpQQ $ \s -> [| runMockQErr emptyMockQ (quoteExp get s) |]
