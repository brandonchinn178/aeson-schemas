{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tests.SchemaQQ.TH
  ( module Tests.SchemaQQ.TH
  , module Tests.SchemaQQ.Types
  ) where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.TestUtils (tryQErr')

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.Internal (ToSchemaObject, showSchemaType)
import Tests.SchemaQQ.Types

-- | A quasiquoter for generating the string representation of a schema.
schemaRep :: QuasiQuoter
schemaRep = QuasiQuoter
  { quoteExp = \s ->
      let schemaType = quoteType schema s
      in [| showSchemaType @(ToSchemaObject $schemaType) |]
  , quoteDec = error "Cannot use `schemaErr` for Dec"
  , quoteType = error "Cannot use `schemaErr` for Type"
  , quotePat = error "Cannot use `schemaErr` for Pat"
  }

schemaErr :: QuasiQuoter
schemaErr = QuasiQuoter
  { quoteExp = \s -> [| $(tryQErr' $ quoteType schema s) :: String |]
  , quoteDec = error "Cannot use `schemaErr` for Dec"
  , quoteType = error "Cannot use `schemaErr` for Type"
  , quotePat = error "Cannot use `schemaErr` for Pat"
  }
