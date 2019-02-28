{-|
Module      :  Data.Aeson.Schema.TH.Get
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'get' quasiquoter.
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Schema.TH.Get (get) where

import Control.Monad ((>=>))
import qualified Data.Maybe as Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)

import Data.Aeson.Schema.Internal (getKey)
import Data.Aeson.Schema.TH.Parse

-- | Defines a QuasiQuoter for expressions.
--
-- > doFoo = do
-- >   result :: Object MySchema <- runQuery ...
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
--     * @SchemaCustom name@ returns a value of the type associated with the given name
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
        GetterBang -> [| $(next) . fromJust $(lift $ mkFromJustMsg history) |]
        GetterMapMaybe -> [| ($(next) <$?>) |]
        GetterMapList -> [| ($(next) <$:>) |]
    mkFromJustMsg history = Maybe.fromMaybe "" start ++ showGetterOps (reverse history)

-- | fromJust with helpful error message
fromJust :: String -> Maybe a -> a
fromJust msg = Maybe.fromMaybe (error errMsg)
  where
    errMsg = "Called 'fromJust' on null expression" ++ if null msg then "" else ": " ++ msg

-- | fmap specialized to Maybe
(<$?>) :: (a -> b) -> Maybe a -> Maybe b
(<$?>) = (<$>)

-- | fmap specialized to [a]
(<$:>) :: (a -> b) -> [a] -> [b]
(<$:>) = (<$>)
