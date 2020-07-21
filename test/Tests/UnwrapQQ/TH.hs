{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.UnwrapQQ.TH
  ( module Tests.UnwrapQQ.TH
  , module Tests.UnwrapQQ.Types
  ) where

import Language.Haskell.TH (appTypeE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema (unwrap)
import Tests.UnwrapQQ.Types
import TestUtils (ShowSchemaResult(..), mkExpQQ)
import TestUtils.MockQ (MockQ(..), emptyMockQ, loadNames, runMockQ, tryMockQ)

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
  in [| runMockQ mockQ (quoteType unwrap s) `seq` $showSchemaResultQ |]

unwrapErr :: QuasiQuoter
unwrapErr = mkExpQQ $ \s -> [|
      case tryMockQ mockQ (quoteType unwrap s) of
        Right a -> error $ "Unexpected success: " ++ show a
        Left e -> e
    |]
