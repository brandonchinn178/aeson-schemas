{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.UnwrapQQ.TH
  ( module Tests.UnwrapQQ.TH
  , module Tests.UnwrapQQ.Types
  ) where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.TestUtils (tryQErr')

import Data.Aeson.Schema (unwrap)
import Tests.UnwrapQQ.Types

unwrapErr :: QuasiQuoter
unwrapErr = QuasiQuoter
  { quoteExp = tryQErr' . quoteType unwrap
  , quoteDec = error "Cannot use `unwrapErr` for Dec"
  , quoteType = error "Cannot use `unwrapErr` for Type"
  , quotePat = error "Cannot use `unwrapErr` for Pat"
  }
