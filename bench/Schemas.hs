{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Schemas
  ( module Schemas
  , module Schemas.TH
  ) where

import Control.Monad (forM)
import Data.Char (chr, toUpper)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH.TestUtils ()

import Schemas.TH

-- Generates:
--
--   type Schema1   = { a1: Int }
--   type Schema5   = { a1: Int, a2: Int, ... a5: Int }
--   type Schema10  = { a1: Int, a2: Int, ... a10: Int }
--   type Schema100 = { a1: Int, a2: Int, ... a100: Int }
$(do
  -- The sizes of schemas to generate
  let schemaSizes = [1, 5, 10, 100]
      allSchemas = flip map schemaSizes $ \n ->
        let name = "Schema" ++ show n
        in (n, name, mkName name)

  concat <$> sequence
    [ forM allSchemas $ \(n, _, name) -> genSchema name $ keysTo n
    , [d|
        sizedSchemas :: [(Int, String)]
        sizedSchemas = $(lift $ flip map allSchemas $ \(n, name, _) -> (n, name))

        sizedSchemasNames :: [(String, Name)]
        sizedSchemasNames = $(lift $ flip map allSchemas $ \(_, name, thName) -> (name, thName))
      |]
    ]
  )
