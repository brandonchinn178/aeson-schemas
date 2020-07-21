{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.UnwrapQQ.TH
  ( module Tests.UnwrapQQ.TH
  , module Tests.UnwrapQQ.Types
  ) where

import Control.DeepSeq (deepseq)
import Language.Haskell.TH (appTypeE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema (unwrap)
import Tests.UnwrapQQ.Types
import TestUtils (ShowSchemaResult(..), mkExpQQ)
import TestUtils.DeepSeq ()
import TestUtils.MockQ (MockQ(..), emptyMockQ, loadNames, runMockQ, runMockQErr)

mockQ :: MockQ
mockQ = emptyMockQ
  { knownNames =
      [ ("ListSchema", ''ListSchema)
      , ("MaybeSchema", ''MaybeSchema)
      , ("SumSchema", ''SumSchema)
      , ("ABCSchema", ''ABCSchema)
      , ("NotASchema", ''Maybe)
      , ("MySchemaResult", ''MySchemaResult)
      ]
  , reifyInfo = $(
      loadNames
        [ ''ListSchema
        , ''MaybeSchema
        , ''SumSchema
        , ''ABCSchema
        , ''Maybe
        , ''MySchema
        , ''MySchemaResult
        ]
    )
  }

-- | A quasiquoter for generating the string representation of an unwrapped schema.
--
-- Also runs the `unwrap` quasiquoter at runtime, to get coverage information.
unwrapRep :: QuasiQuoter
unwrapRep = mkExpQQ $ \s ->
  let showSchemaResultQ = appTypeE [| showSchemaResult |] (quoteType unwrap s)
  in [| runMockQ mockQ (quoteType unwrap s) `deepseq` $showSchemaResultQ |]

unwrapErr :: QuasiQuoter
unwrapErr = mkExpQQ $ \s -> [| runMockQErr mockQ (quoteType unwrap s) |]
