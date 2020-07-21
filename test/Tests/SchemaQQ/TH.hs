{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tests.SchemaQQ.TH
  ( module Tests.SchemaQQ.TH
  , module Tests.SchemaQQ.Types
  ) where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift, reify)
import Language.Haskell.TH.TestUtils (tryQErr')

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.Internal (ToSchemaObject, showSchemaType)
import Tests.SchemaQQ.Types
import TestUtils.MockQ (MockQ(..), emptyMockQ, runMockQ)

-- | A quasiquoter for generating the string representation of a schema.
--
-- Also runs the `schema` quasiquoter at runtime, to get coverage information.
schemaRep :: QuasiQuoter
schemaRep = QuasiQuoter
  { quoteExp = \s ->
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
            , reifyInfo =
                [ (''ExtraSchema, $(reify ''ExtraSchema >>= lift))
                ]
            }
      in [| runMockQ mockQ (quoteType schema s) `seq` showSchemaType @(ToSchemaObject $schemaType) |]
  , quoteDec = error "Cannot use `schemaRep` for Dec"
  , quoteType = error "Cannot use `schemaRep` for Type"
  , quotePat = error "Cannot use `schemaRep` for Pat"
  }

schemaErr :: QuasiQuoter
schemaErr = QuasiQuoter
  { quoteExp = \s -> [| $(tryQErr' $ quoteType schema s) :: String |]
  , quoteDec = error "Cannot use `schemaErr` for Dec"
  , quoteType = error "Cannot use `schemaErr` for Type"
  , quotePat = error "Cannot use `schemaErr` for Pat"
  }
