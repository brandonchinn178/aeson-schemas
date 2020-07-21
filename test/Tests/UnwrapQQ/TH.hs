{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tests.UnwrapQQ.TH
  ( module Tests.UnwrapQQ.TH
  , module Tests.UnwrapQQ.Types
  ) where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift, reify)

import Data.Aeson.Schema (unwrap)
import Tests.UnwrapQQ.Types
import TestUtils (ShowSchemaResult(..))
import TestUtils.MockQ (MockQ(..), emptyMockQ, runMockQ, tryMockQ)

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
  , reifyInfo =
      [ (''ListSchema, $(reify ''ListSchema >>= lift))
      , (''MaybeSchema, $(reify ''MaybeSchema >>= lift))
      , (''SumSchema, $(reify ''SumSchema >>= lift))
      , (''ABCSchema, $(reify ''ABCSchema >>= lift))
      , (''Maybe, $(reify ''Maybe >>= lift))
      , (''MySchema, $(reify ''MySchema >>= lift))
      , (''MySchemaResult, $(reify ''MySchemaResult >>= lift))
      ]
  }

-- | A quasiquoter for generating the string representation of an unwrapped schema.
--
-- Also runs the `unwrap` quasiquoter at runtime, to get coverage information.
unwrapRep :: QuasiQuoter
unwrapRep = QuasiQuoter
  { quoteExp = \s ->
      [| runMockQ mockQ (quoteType unwrap s) `seq` showSchemaResult @($(quoteType unwrap s)) |]
  , quoteDec = error "Cannot use `unwrapRep` for Dec"
  , quoteType = error "Cannot use `unwrapRep` for Type"
  , quotePat = error "Cannot use `unwrapRep` for Pat"
  }

unwrapErr :: QuasiQuoter
unwrapErr = QuasiQuoter
  { quoteExp = \s -> [|
      case tryMockQ mockQ (quoteType unwrap s) of
        Right a -> error $ "Unexpected success: " ++ show a
        Left e -> e
    |]
  , quoteDec = error "Cannot use `unwrapErr` for Dec"
  , quoteType = error "Cannot use `unwrapErr` for Type"
  , quotePat = error "Cannot use `unwrapErr` for Pat"
  }
