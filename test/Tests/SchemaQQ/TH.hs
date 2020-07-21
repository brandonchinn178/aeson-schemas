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
import TestUtils (mkExpQQ)
import TestUtils.MockQ (MockQ(..), emptyMockQ, loadNames, runMockQ)

-- | A quasiquoter for generating the string representation of a schema.
--
-- Also runs the `schema` quasiquoter at runtime, to get coverage information.
schemaRep :: QuasiQuoter
schemaRep = mkExpQQ $ \s ->
  let schemaType = quoteType schema s
      mockQ = emptyMockQ
        { knownNames =
            [ ("Status", ''Status)
            , ("UserSchema", ''UserSchema)
            , ("ExtraSchema", ''ExtraSchema)
            , ("ExtraSchema2", ''ExtraSchema2)
            , ("Tests.SchemaQQ.TH.UserSchema", ''UserSchema)
            , ("Tests.SchemaQQ.TH.ExtraSchema", ''ExtraSchema)
            ]
        , reifyInfo = $(loadNames [''ExtraSchema])
        }
  in [| runMockQ mockQ (quoteType schema s) `seq` showSchemaType @(ToSchemaObject $schemaType) |]

schemaErr :: QuasiQuoter
schemaErr = mkExpQQ $ \s -> [| $(tryQErr' $ quoteType schema s) :: String |]
