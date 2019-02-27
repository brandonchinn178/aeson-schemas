{-|
Module      :  Data.Aeson.Schema.TH.Get
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'get' quasiquoter.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Schema.TH.Get where

import Control.Monad (unless, (>=>))
import qualified Data.Maybe as Maybe
import GHC.Stack (HasCallStack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)

import Data.Aeson.Schema.Internal (getKey)
import Data.Aeson.Schema.TH.Parse (GetterExp(..), getterExp, parse)
import Data.Aeson.Schema.TH.Utils (GetterOperation(..), showGetterOps)

-- | Defines a QuasiQuoter for extracting JSON data.
--
-- Example:
--
-- > let Just result = decode ... :: Maybe (Object MySchema)
-- >
-- > [get| result.foo.a |]          :: Int
-- > [get| result.foo.nodes |]      :: [Object (..)]
-- > [get| result.foo.nodes[] |]    :: [Object (..)]
-- > [get| result.foo.nodes[].b |]  :: [Maybe Bool]
-- > [get| result.foo.nodes[].b! |] :: [Bool] -- runtime error if any values are Nothing
-- > [get| result.foo.c |]          :: Text
-- > [get| result.foo.(a,c) |]      :: (Int, Text)
-- > [get| result.foo.[c,d] |]      :: [Text]
-- >
-- > let nodes = [get| result.foo.nodes |]
-- > flip map nodes $ \node -> fromMaybe ([get| node.num |] == 0) [get| node.b |]
-- > map [get| .num |] nodes
--
-- Syntax:
--
-- * @x.y@ is only valid if @x@ is an 'Data.Aeson.Schema.Object'. Returns the value of the key @y@.
--
-- * @.y@ returns a function that takes in an 'Data.Aeson.Schema.Object' and returns the value of
--   the key @y@.
--
-- * @x.[y,z.a]@ is only valid if @x@ is an 'Data.Aeson.Schema.Object', and if @y@ and @z.a@ have
--   the same type. Returns the value of the operations @y@ and @z.a@ as a list.
--   MUST be the last operation.
--
-- * @x.(y,z.a)@ is only valid if @x@ is an 'Data.Aeson.Schema.Object'. Returns the value of the
--   operations @y@ and @z.a@ as a tuple.
--   MUST be the last operation.
--
-- * @x!@ is only valid if @x@ is a 'Maybe'. Unwraps the value of @x@ from a 'Just' value and
--   errors (at runtime!) if @x@ is 'Nothing'.
--
-- * @x[]@ is only valid if @x@ is a list. Applies the remaining rules as an 'fmap' over the
--   values in the list, e.g.
--
--     * @x[]@ without anything after is equivalent to @x@
--     * @x[].y@ gets the key @y@ in all the Objects in @x@
--     * @x[]!@ unwraps all 'Just' values in @x@ (and errors if any 'Nothing' values exist in @x@)
--
-- * @x?@ follows the same rules as @x[]@ except it's only valid if @x@ is a 'Maybe'.
get :: QuasiQuoter
get = QuasiQuoter
  { quoteExp = parse getterExp >=> generateGetterExp
  , quoteDec = error "Cannot use `get` for Dec"
  , quoteType = error "Cannot use `get` for Type"
  , quotePat = error "Cannot use `get` for Pat"
  }

generateGetterExp :: GetterExp -> ExpQ
generateGetterExp GetterExp{..} = maybe expr (appE expr . varE . mkName) start
  where
    startDisplay = case start of
      Nothing -> ""
      Just s -> if '.' `elem` s then "(" ++ s ++ ")" else s
    expr = mkGetterExp [] getterOps

    applyToNext next = \case
      Right f -> [| $next . $f |]
      Left f -> infixE (Just next) f Nothing

    applyToEach history fromElems elems = do
      val <- newName "v"
      let mkElem ops = appE (mkGetterExp history ops) (varE val)
      lamE [varP val] $ fromElems $ map mkElem elems

    mkGetterExp history = \case
      [] -> [| id |]
      op:ops ->
        let applyToNext' = applyToNext $ mkGetterExp (op:history) ops
            applyToEach' = applyToEach history
            checkLast label = unless (null ops) $ fail $ label ++ " operation MUST be last."
            fromJustMsg = startDisplay ++ showGetterOps (reverse history)
        in case op of
          GetterKey key     -> applyToNext' $ Right $ appTypeE [| getKey |] (litT $ strTyLit key)
          GetterList elems  -> checkLast ".[*]" >> applyToEach' listE elems
          GetterTuple elems -> checkLast ".(*)" >> applyToEach' tupE elems
          GetterBang        -> applyToNext' $ Right [| fromJust $(lift fromJustMsg) |]
          GetterMapMaybe    -> applyToNext' $ Left [| (<$?>) |]
          GetterMapList     -> applyToNext' $ Left [| (<$:>) |]

-- | fromJust with helpful error message
fromJust :: HasCallStack => String -> Maybe a -> a
fromJust msg = Maybe.fromMaybe (error errMsg)
  where
    errMsg = "Called 'fromJust' on null expression" ++ if null msg then "" else ": " ++ msg

-- | fmap specialized to Maybe
(<$?>) :: (a -> b) -> Maybe a -> Maybe b
(<$?>) = (<$>)

-- | fmap specialized to [a]
(<$:>) :: (a -> b) -> [a] -> [b]
(<$:>) = (<$>)
