{-|
Module      :  Data.Aeson.Schema.TH.Unwrap
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'unwrap' quasiquoter.
-}
{-# LANGUAGE RecordWildCards #-}

module Data.Aeson.Schema.TH.Unwrap where

import Control.Monad ((>=>))
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema.TH.Parse (UnwrapSchema(..), parse, unwrapSchema)
import Data.Aeson.Schema.TH.Utils (reifySchema, unwrapType)

-- | Defines a QuasiQuoter to extract a schema within the given schema.
--
-- For example:
--
-- > -- | MyFoo ~ Object [schema| { b: Maybe Bool } |]
-- > type MyFoo = [unwrap| MySchema.foo.nodes[] |]
--
-- If the schema is imported qualified, you can use parentheses to distinguish it from the
-- expression:
--
-- > type MyFoo = [unwrap| (MyModule.Schema).foo.nodes[] |]
--
-- You can then use the type alias as usual:
--
-- > parseBar :: MyFoo -> String
-- > parseBar = maybe "null" show . [get| .b |]
-- >
-- > foo = map parseBar [get| result.foo.nodes[] |]
--
-- The syntax is mostly the same as 'Data.Aeson.Schema.TH.get', except the operations run on the
-- type itself, instead of the values. Differences from 'Data.Aeson.Schema.TH.get':
--
-- * @x!@ is only valid if @x@ is a @Maybe a@ type. Returns @a@, the type wrapped in the 'Maybe'.
--
-- * @x?@ is the same as @x!@.
--
-- * @x[]@ is only valid if @x@ is a @[a]@ type. Returns @a@, the type contained in the list.
--
-- * @x\@#@ is only valid if @x@ is a @SumType@. Returns the type at that branch in the sum type.
unwrap :: QuasiQuoter
unwrap = QuasiQuoter
  { quoteExp = error "Cannot use `unwrap` for Exp"
  , quoteDec = error "Cannot use `unwrap` for Dec"
  , quoteType = parse unwrapSchema >=> generateUnwrapSchema
  , quotePat = error "Cannot use `unwrap` for Pat"
  }

generateUnwrapSchema :: UnwrapSchema -> TypeQ
generateUnwrapSchema UnwrapSchema{..} = do
  startSchemaName <- maybe unknownSchema return =<< lookupTypeName startSchema
  startSchemaType <- reifySchema startSchemaName
  unwrapType False getterOps startSchemaType
  where
    unknownSchema = fail $ "Unknown schema: " ++ startSchema
