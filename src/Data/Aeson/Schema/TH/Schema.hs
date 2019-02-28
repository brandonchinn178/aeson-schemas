{-|
Module      :  Data.Aeson.Schema.TH.Schema
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'schema' quasiquoter.
-}

module Data.Aeson.Schema.TH.Schema (schema) where

import Language.Haskell.TH.Quote (QuasiQuoter)

-- | Defines a QuasiQuoter for writing schemas.
--
-- Example:
--
-- > import Data.Aeson.Schema (SchemaGraph(..))
-- > import Data.Aeson.Schema.TH (schema)
-- >
-- > -- | MySchema ~ 'SchemaObject
-- > --     '[ '("a", 'SchemaInt)
-- > --      , '("nodes", 'SchemaList ('SchemaObject
-- > --           '[ '("b", 'SchemaMaybe 'SchemaBool)
-- > --            ]
-- > --         ))
-- > --      , '("c", 'SchemaText)
-- > --      , '("d", 'SchemaText)
-- > --      ]
-- > type MySchema = [schema|
-- >   {
-- >     "foo": {
-- >        "a": Int,
-- >        "nodes": List {
-- >           "b": Maybe Bool,
-- >        },
-- >        "c": Text,
-- >        "d": Text,
-- >     },
-- >   }
-- > |]
schema :: QuasiQuoter
schema = undefined
