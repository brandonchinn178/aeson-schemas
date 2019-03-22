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
-- > -- | MyFoo ~ 'SchemaObject '[ '("b", 'SchemaMaybe 'SchemaBool) ]
-- > type MyFoo = [unwrap| MySchema.foo.nodes[] |]
--
-- If the schema is imported qualified, you can use parentheses to distinguish:
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
-- The available operations mostly correspond to 'Data.Aeson.Schema.TH.get', except the operations
-- are on the schema itself instead of the values. The primary difference here is that operations
-- that would be fmapped in a 'Data.Aeson.Schema.TH.get' expression strip out the functor type; e.g.
-- @int_list[]@ would return @[Int]@ with 'Data.Aeson.Schema.TH.get', but would return @Int@ with
-- 'unwrap'.
--
-- * @x@ returns the type of @x@ with the given schema:
--
--     * @SchemaBool@ returns a 'Bool'
--     * @SchemaInt@ returns an 'Int'
--     * @SchemaDouble@ returns a 'Double'
--     * @SchemaText@ returns a 'Text.Text'
--     * @SchemaCustom name@ returns a value of the type associated with the given name
--     * @SchemaMaybe schema@ returns a 'Maybe' value wrapping the value returned by the inner schema
--     * @SchemaList schema@ returns a list of values, whose type is determined by the inner schema
--     * @SchemaObject fields@ returns an 'Data.Aeson.Schema.Object' with the given schema
--
-- * @x.y@ is only valid if @x@ is a @SchemaObject@. Returns the type of the key @y@ in the
--   'Data.Aeson.Schema.Object'.
--
-- * @x.[y,z.a]@ is only valid if @x@ is a @SchemaObject@, and if @y@ and @z.a@ have the same schema.
--   Returns the type of the operations @y@ and @z.a@ in the 'Data.Aeson.Schema.Object' as a list.
--
-- * @x.(y,z.a)@ is only valid if @x@ is a @SchemaObject@. Returns the type of the operations @y@
--   and @z.a@ in the 'Data.Aeson.Schema.Object' as a tuple.
--
-- * @x!@ is only valid if @x@ is a @SchemaMaybe a@. Returns @a@, the type wrapped in the 'Maybe'.
--
-- * @x[]@ is only valid if @x@ is a @SchemaList a@. Returns @a@, the type contained in the list.
--
-- * @x?@ is the same as @x!@.
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
