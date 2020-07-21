{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tests.SchemaQQ.TH
  ( module Tests.SchemaQQ.TH
  , module Tests.SchemaQQ.Types
  ) where

import Control.DeepSeq (deepseq)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.Internal (ToSchemaObject, showSchemaType)
import Tests.SchemaQQ.Types
import TestUtils (mkExpQQ)
import TestUtils.DeepSeq ()
import TestUtils.MockQ (MockQ(..), emptyMockQ, loadNames, runMockQ, runMockQErr)

mockQ :: MockQ
mockQ = emptyMockQ
  { knownNames =
      [ ("Status", ''Status)
      , ("UserSchema", ''UserSchema)
      , ("ExtraSchema", ''ExtraSchema)
      , ("ExtraSchema2", ''ExtraSchema2)
      , ("Tests.SchemaQQ.TH.UserSchema", ''UserSchema)
      , ("Tests.SchemaQQ.TH.ExtraSchema", ''ExtraSchema)
      , ("Int", ''Int)
      ]
  , reifyInfo = $(loadNames [''ExtraSchema, ''ExtraSchema2, ''Int])
  }

-- | A quasiquoter for generating the string representation of a schema.
--
-- Also runs the `schema` quasiquoter at runtime, to get coverage information.
schemaRep :: QuasiQuoter
schemaRep = mkExpQQ $ \s ->
  let schemaType = quoteType schema s
  in [| runMockQ mockQ (quoteType schema s) `deepseq` showSchemaType @(ToSchemaObject $schemaType) |]

schemaErr :: QuasiQuoter
schemaErr = mkExpQQ $ \s -> [| runMockQErr mockQ (quoteType schema s) |]
