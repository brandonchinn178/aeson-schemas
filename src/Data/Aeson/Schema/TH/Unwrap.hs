{-|
Module      :  Data.Aeson.Schema.TH.Unwrap
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'get' quasiquoter.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Schema.TH.Unwrap (unwrap) where

import Control.Monad ((>=>))
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema.Internal (Object, SchemaResult, SchemaType(..))
import Data.Aeson.Schema.TH.Parse
import Data.Aeson.Schema.TH.Utils (showSchemaType, stripSigs)

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
-- The available operations mostly correspond to 'get', except the operations are on the schema
-- itself instead of the values:
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
--     * @SchemaObject fields@ returns an 'Data.Aeson.Schema.Object'
--
-- * @x.y@ is only valid if @x@ is a @SchemaObject@. Returns the type of the key @y@ in the 'Object'.
--
-- * @x.[y,z.a]@ is only valid if @x@ is a @SchemaObject@, and if @y@ and @z.a@ have the same schema.
--   Returns the type of the operations @y@ and @z.a@ in the 'Object' as a list.
--
-- * @x.(y,z.a)@ is only valid if @x@ is a @SchemaObject@. Returns the type of the operations @y@
--   and @z.a@ in the 'Object' as a tuple.
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
  startSchemaName <- maybe (fail $ "Unknown schema: " ++ startSchema) return =<< lookupTypeName startSchema
  startSchemaType <- reify startSchemaName >>= \case
    TyConI (TySynD _ _ ty) -> return $ stripSigs ty
    info -> fail $ "Unknown type to unwrap: " ++ show info
  getType startSchemaType getterOps
  where
    getType schema [] = fromSchemaType schema
    getType schema (op:ops) = case schema of
      AppT (PromotedT ty) inner ->
        case op of
          GetterKey key | ty == 'SchemaObject ->
            case lookup key (getObjectSchema inner) of
              Just schema' -> getType schema' ops
              Nothing -> fail $ "Key '" ++ key ++ "' does not exist in schema: " ++ showSchemaType schema
          GetterKey key -> fail $ "Cannot get key '" ++ key ++ "' in schema: " ++ showSchemaType schema
          GetterList elems | ty == 'SchemaObject -> do
            (elem':rest) <- mapM (getType schema) elems
            if all (== elem') rest
              then getType elem' ops
              else fail $ "List contains different types with schema: " ++ showSchemaType schema
          GetterList _ -> fail $ "Cannot get keys in schema: " ++ showSchemaType schema
          GetterTuple elems | ty == 'SchemaObject ->
            foldl appT (tupleT $ length elems) $ map (getType schema) elems
          GetterTuple _ -> fail $ "Cannot get keys in schema: " ++ showSchemaType schema
          GetterBang | ty == 'SchemaMaybe -> getType inner ops
          GetterBang -> fail $ "Cannot use `!` operator on schema: " ++ showSchemaType schema
          GetterMapMaybe | ty == 'SchemaMaybe -> getType inner ops
          GetterMapMaybe -> fail $ "Cannot use `?` operator on schema: " ++ showSchemaType schema
          GetterMapList | ty == 'SchemaList -> getType inner ops
          GetterMapList -> fail $ "Cannot use `[]` operator on schema: " ++ showSchemaType schema
      -- allow starting from (Object schema)
      AppT (ConT ty) inner | ty == ''Object -> getType inner (op:ops)
      _ -> fail $ unlines ["Cannot get type:", show schema, show op]
    getObjectSchema schema = case schema of
      AppT (AppT PromotedConsT t1) t2 ->
        case t1 of
          AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit key))) ty -> (key, ty) : getObjectSchema t2
          _ -> error $ "Could not parse a (key, schema) tuple: " ++ show t1
      PromotedNilT -> []
      t -> error $ "Could not get object schema: " ++ show t
    fromSchemaType schema = case schema of
      schema'@(AppT (PromotedT ty) inner)
        | ty == 'SchemaCustom -> [t| SchemaResult $(pure schema') |]
        | ty == 'SchemaMaybe -> [t| Maybe $(fromSchemaType inner) |]
        | ty == 'SchemaList -> [t| [$(fromSchemaType inner)] |]
        | ty == 'SchemaObject -> [t| Object $(pure schema) |]
      PromotedT ty
        | ty == 'SchemaBool -> [t| Bool |]
        | ty == 'SchemaInt -> [t| Int |]
        | ty == 'SchemaDouble -> [t| Double |]
        | ty == 'SchemaText -> [t| Text |]
      AppT t1 t2 -> appT (fromSchemaType t1) (fromSchemaType t2)
      TupleT _ -> pure schema
      _ -> fail $ "Could not convert schema: " ++ showSchemaType schema
