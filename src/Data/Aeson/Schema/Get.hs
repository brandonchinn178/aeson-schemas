{-|
Module      :  Data.Aeson.Schema.Get
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Provides an alternative method of extracting data from JSON objects without quasiquoters.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Aeson.Schema.Get
  ( -- * ComposeFunctors API
    get
  , apply
  , into
  , intoMaybe
  , intoList
  , key
  , bang
  , ComposeFunctors
  ) where

import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.OverloadedLabels (IsLabel(..))
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, Symbol)

import Data.Aeson.Schema.Internal (LookupSchema, Object, SchemaResult, getKey)
import Data.Aeson.Schema.Type (Schema, SchemaType)

-- | Use 'ComposeFunctors' operations to extract values from 'Object' values, without the use
-- of quasiquoters.
--
-- Can use OverloadedLabels to extract keys. e.g.
--
-- > get (key #name . intoList . key #users)
--
-- is equivalent to
--
-- > [get| .users[].name |]
get :: CanComposeFunctors fs b => (ComposeFunctors '[] a -> ComposeFunctors fs b) -> a -> ResolveFunctors fs b
get f = resolve . f . Pure

-- | Lift the given function to 'ComposeFunctors'.
--
-- Basically an alias for 'fmap'.
apply :: Functor (ComposeFunctors fs) => (a -> b) -> ComposeFunctors fs a -> ComposeFunctors fs b
apply = fmap

-- | For a value @f a@, indicate that following expressions operate on @a@.
--
-- For example, @fmap (fmap g) . f@ could be rewritten as @get (apply g . into . into . apply f)@
into :: Functor f => ComposeFunctors fs (f a) -> ComposeFunctors (f ': fs) a
into = Into

-- | 'into' specialized for lists.
intoList :: ComposeFunctors fs [a] -> ComposeFunctors ([] ': fs) a
intoList = into

-- | 'into' specialized for Maybes.
intoMaybe :: ComposeFunctors fs (Maybe a) -> ComposeFunctors (Maybe ': fs) a
intoMaybe = into

-- instance
--   ( endSchema ~ LookupSchema key schema
--   , result ~ SchemaResult endSchema
--   , KnownSymbol key
--   , Typeable result
--   , Typeable endSchema
--   , Functor (ComposeFunctors fs)
--   ) => IsLabel key (ComposeFunctors fs (Object schema) -> ComposeFunctors fs result) where
--   fromLabel = apply (getKey $ Proxy @key)

instance (KnownSymbol key1, key1 ~ key2) => IsLabel key1 (Proxy key2) where
  fromLabel = Proxy @key2

-- | 'getKey' lifted to 'ComposeFunctors'.
--
-- May be used with OverloadedLabels:
--
-- > get (key #users) o == getKey (Proxy @"users") o
key
  :: forall (key :: Symbol) (schema :: Schema) (endSchema :: SchemaType) result fs.
     ( endSchema ~ LookupSchema key schema
     , result ~ SchemaResult endSchema
     , KnownSymbol key
     , Typeable result
     , Typeable endSchema
     , Functor (ComposeFunctors fs)
     )
  => Proxy key
  -> ComposeFunctors fs (Object schema)
  -> ComposeFunctors fs result
key proxy = apply (getKey proxy)

-- | 'fromJust' lifted to 'ComposeFunctors'.
bang :: (HasCallStack, Functor (ComposeFunctors fs)) => ComposeFunctors fs (Maybe a) -> ComposeFunctors fs a
bang = apply fromJust

{- ComposeFunctors helper -}

-- | The data type containing a list of functors to apply onto the given value.
--
-- A value of type @ComposeFunctors '[f1, f2] a@ can be thought of as equivalent
-- to @f2 (f1 a)@. @ComposeFunctors@ has a @Functor@ instance that applies a function
-- through the functors.
--
-- As an example, (using @List@ instead of @[]@ for clarity):
--
-- > Pure [Just 1]                   :: ComposeFunctors '[] (List (Maybe Int))
-- > Into (Pure [Just 1])          :: ComposeFunctors '[List] (Maybe Int)
-- > Into (Into (Pure [Just 1])) :: ComposeFunctors '[Maybe, List] Int
-- >
-- > show <$> Into (Into (Pure [Just 1])) == Into (Into (Pure [Just "1"]))
-- >   :: ComposeFunctors '[Maybe, List] String
data ComposeFunctors (fs :: [Type -> Type]) a where
  Pure :: a -> ComposeFunctors '[] a
  Into :: Functor f => ComposeFunctors fs (f a) -> ComposeFunctors (f ': fs) a

instance Functor (ComposeFunctors '[]) where
  fmap f (Pure x) = Pure $ f x

instance (Functor f, Functor (ComposeFunctors fs)) => Functor (ComposeFunctors (f ': fs)) where
  fmap f (Into inner) = Into $ fmap f <$> inner

class CanComposeFunctors fs a where
  type ResolveFunctors fs a
  resolve :: ComposeFunctors fs a -> ResolveFunctors fs a

instance CanComposeFunctors '[] a where
  type ResolveFunctors '[] a = a
  resolve (Pure result) = result

instance CanComposeFunctors fs (f a) => CanComposeFunctors (f ': fs) a where
  type ResolveFunctors (f ': fs) a = ResolveFunctors fs (f a)
  resolve (Into result) = resolve result
