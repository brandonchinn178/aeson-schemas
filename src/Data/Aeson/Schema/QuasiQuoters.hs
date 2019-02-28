{-|
Module      :  Data.Aeson.Schema.QuasiQuoters
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for querying JSON data using quasiquoters.

'Data.Aeson.Schema.SchemaGraph' defines the shape of the JSON object stored in
'Data.Aeson.Schema.Object', and we can use 'Data.Aeson.Schema.Internal.getKey' to lookup a key that
is checked at compile-time to exist in the object.

To make it easier to extract deeply nested keys, this module defines QuasiQuoters that generate the
corresponding 'Data.Aeson.Schema.Internal.getKey' expressions.

In addition to the QuasiQuotes extension, the following extensions will need to be enabled to
use these QuasiQuoters:

* DataKinds
* FlexibleContexts
* TypeFamilies
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Schema.QuasiQuoters
  ( get
  , unwrap
  ) where

import Control.Monad ((>=>))
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)

import Data.Aeson.Schema.Internal
    (Object, SchemaGraph(..), SchemaResult, getKey)
import Data.Aeson.Schema.QuasiQuoters.Parse

-- | Defines a QuasiQuoter for expressions.
--
-- > doFoo = do
-- >   result <- runQuery ...
-- >
-- >   [get| result.foo.a |]          :: Int
-- >   [get| result.foo.nodes |]      :: [Object (..)]
-- >   [get| result.foo.nodes[] |]    :: [Object (..)]
-- >   [get| result.foo.nodes[].b |]  :: [Maybe Bool]
-- >   [get| result.foo.nodes[].b! |] :: [Bool] -- runtime error if any "b" values are null
-- >   [get| result.foo.c |]          :: Text
-- >   [get| result.foo.(a,c) |]      :: (Int, Text)
-- >   [get| result.foo.[c,d] |]      :: [Text]
-- >
-- >   let nodes = [get| result.foo.nodes |]
-- >   flip map nodes $ \node -> fromMaybe ([get| node.num |] == 0) [get| node.b |]
-- >   map [get| .num |] nodes
--
-- These "getter" expressions follow the given rules:
--
-- * @x@ returns the value of @x@ with the given type:
--
--     * @SchemaBool@ returns a 'Bool'
--     * @SchemaInt@ returns an 'Int'
--     * @SchemaDouble@ returns a 'Double'
--     * @SchemaText@ returns a 'Text.Text'
--     * @SchemaScalar name@ returns a value of the type associated with the given name
--     * @SchemaEnum name@ returns a value of the type associated with the given name
--     * @SchemaMaybe schema@ returns a 'Maybe' value wrapping the value returned by the inner schema
--     * @SchemaList schema@ returns a list of values, whose type is determined by the inner schema
--     * @SchemaObject fields@ returns an 'Data.Aeson.Schema.Object'
--
-- * @x.y@ is only valid if @x@ is a @SchemaObject@. Returns the value of the key @y@ in the 'Object'.
--
-- * @x.[y,z.a]@ is only valid if @x@ is a @SchemaObject@, and if @y@ and @z.a@ have the same schema.
--   Returns the value of the operations @y@ and @z.a@ in the 'Object' as a list.
--
-- * @x.(y,z.a)@ is only valid if @x@ is a @SchemaObject@. Returns the value of the operations @y@
--   and @z.a@ in the 'Object' as a tuple.
--
-- * @x!@ is only valid if @x@ is a @SchemaMaybe@. Unwraps the value of @x@ from a 'Just' value and
--   errors (at runtime!) if @x@ is 'Nothing'.
--
-- * @x[]@ is only valid if @x@ is a @SchemaList@. Applies the remaining rules as an 'fmap' over the
--   values in the list.
--
--     * @x[]@ without anything after is equivalent to @x@
--     * @x[].y@ gets the key @y@ in all the Objects in @x@
--     * @x[]!@ unwraps all 'Just' values in @x@ (and errors if any 'Nothing' values exist in @x@)
--
-- * @x?@ follows the same rules as @x[]@ except it's only valid if @x@ is a @SchemaMaybe@.
get :: QuasiQuoter
get = QuasiQuoter
  { quoteExp = parse getterExp >=> generateGetterExp
  , quoteDec = error "Cannot use `get` for Dec"
  , quoteType = error "Cannot use `get` for Type"
  , quotePat = error "Cannot use `get` for Pat"
  }

generateGetterExp :: GetterExp -> ExpQ
generateGetterExp GetterExp{..} =
  case start of
    Nothing -> do
      arg <- newName "x"
      lamE [varP arg] (apply arg)
    Just arg -> apply $ mkName arg
  where
    apply = appE (mkGetter [] getterOps) . varE
    mkGetter _ [] = [| id |]
    mkGetter history (op:ops) =
      let next = mkGetter (op : history) ops
          applyValToOps val = map ((`appE` varE val) . mkGetter history)
      in case op of
        GetterKey key ->
          let getKey' = appTypeE [|getKey|] (litT $ strTyLit key)
          in [| $(next) . $(getKey') |]
        GetterList elems -> do
          val <- newName "v"
          lamE [varP val] (listE $ applyValToOps val elems)
        GetterTuple elems -> do
          val <- newName "v"
          lamE [varP val] (tupE $ applyValToOps val elems)
        GetterBang -> [| $(next) . fromJust $(lift start) $(lift $ reverse history) |]
        GetterMapMaybe -> [| ($(next) <$?>) |]
        GetterMapList -> [| ($(next) <$:>) |]

-- | fromJust with helpful error message
fromJust :: Maybe String -> GetterOps -> Maybe a -> a
fromJust start ops =
  if null start' && null ops
    then fromJust' ""
    else fromJust' $ ": " ++ start' ++ showGetterOps ops
  where
    start' = Maybe.fromMaybe "" start
    fromJust' = Maybe.fromMaybe . error . ("Called 'fromJust on null expression" ++)

-- | fmap specialized to Maybe
(<$?>) :: (a -> b) -> Maybe a -> Maybe b
(<$?>) = (<$>)

-- | fmap specialized to [a]
(<$:>) :: (a -> b) -> [a] -> [b]
(<$:>) = (<$>)

-- | Defines a QuasiQuoter to extract a schema within the given schema.
--
-- For example, the following code
--
-- > type MySchema = 'SchemaObject
-- >   '[ '("foo", 'SchemaList ('SchemaObject
-- >        '[ '("bar", 'SchemaText)
-- >         ]
-- >       ))
-- >    ]
-- >
-- > type MyFoo = [unwrap| MySchema.foo[] |]
--
-- defines @MyFoo@ to be @'SchemaObject '[ '("bar", 'SchemaText) ]@. If the schema is imported
-- qualified, you can use parentheses to distinguish:
--
-- > type MyFoo = [unwrap| (MyModule.Schema).foo[] |]
--
-- You can then use the type alias as usual:
--
-- > parseBar :: Object MyFoo -> [Text]
-- > parseBar = Text.splitOn "," . [get| .bar |]
-- >
-- > foo = map parseBar [get| result.foo[] |]
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
--     * @SchemaScalar name@ returns a value of the type associated with the given name
--     * @SchemaEnum name@ returns a value of the type associated with the given name
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
    TyConI (TySynD _ _ ty) -> return ty
    info -> fail $ "Unknown type to unwrap: " ++ show info
  getType startSchemaType getterOps
  where
    unSig = \case
      SigT ty _ -> ty
      ty -> ty
    getType schema [] = fromSchemaType schema
    getType schema (op:ops) = case unSig schema of
      AppT (PromotedT ty) inner ->
        case op of
          GetterKey key | ty == 'SchemaObject ->
            case lookup key (getObjectSchema inner) of
              Just schema' -> getType schema' ops
              Nothing -> fail $ "Key '" ++ key ++ "' does not exist in schema: " ++ show schema
          GetterKey key -> fail $ "Cannot get key '" ++ key ++ "' in schema: " ++ show schema
          GetterList elems | ty == 'SchemaObject -> do
            (elem':rest) <- mapM (getType schema) elems
            if all (== elem') rest
              then getType elem' ops
              else fail $ "List contains different types with schema: " ++ show schema
          GetterList _ -> fail $ "Cannot get keys in schema: " ++ show schema
          GetterTuple elems | ty == 'SchemaObject ->
            foldl appT (tupleT $ length elems) $ map (getType schema) elems
          GetterTuple _ -> fail $ "Cannot get keys in schema: " ++ show schema
          GetterBang | ty == 'SchemaMaybe -> getType inner ops
          GetterBang -> fail $ "Cannot use `!` operator on schema: " ++ show schema
          GetterMapMaybe | ty == 'SchemaMaybe -> getType inner ops
          GetterMapMaybe -> fail $ "Cannot use `?` operator on schema: " ++ show schema
          GetterMapList | ty == 'SchemaList -> getType inner ops
          GetterMapList -> fail $ "Cannot use `[]` operator on schema: " ++ show schema
      _ -> fail $ unlines ["Cannot get type:", show schema, show op]
    getObjectSchema schema = case unSig schema of
      AppT (AppT PromotedConsT t1) t2 ->
        case unSig t1 of
          AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit key))) ty -> (key, ty) : getObjectSchema t2
          _ -> error $ "Could not parse a (key, schema) tuple: " ++ show t1
      PromotedNilT -> []
      t -> error $ "Could not get object schema: " ++ show t
    fromSchemaType schema = case unSig schema of
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
      _ -> fail $ "Could not convert schema: " ++ show schema
