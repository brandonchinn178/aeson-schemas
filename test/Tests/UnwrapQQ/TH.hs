{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.UnwrapQQ.TH
  ( module Tests.UnwrapQQ.TH
  , module Tests.UnwrapQQ.Types
  ) where

import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter(quoteType))
import Language.Haskell.TH.TestUtils (tryQErr')

import Data.Aeson.Schema (unwrap)
import Tests.UnwrapQQ.Types

getUnwrapQQErr :: String -> ExpQ
getUnwrapQQErr = tryQErr' . quoteType unwrap
