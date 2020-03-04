{-|
Module      :  Data.Aeson.Schema.Utils.Sum
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'SumType' data type that represents a sum type consisting of types
specified in a type-level list.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aeson.Schema.Utils.Sum (SumType(..)) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..))
import Data.Kind (Type)

-- | Represents a sum type.
--
-- Loads the first type that successfully parses the JSON value.
--
-- Example:
--
-- @
-- data Owl = Owl
-- data Cat = Cat
-- data Toad = Toad
-- type Animal = SumType '[Owl, Cat, Toad]
--
-- Here Owl                         :: Animal
-- There (Here Cat)                 :: Animal
-- There (There (Here Toad))        :: Animal
--
-- {- Fails at compile-time
-- Here True                        :: Animal
-- Here Cat                         :: Animal
-- There (Here Owl)                 :: Animal
-- There (There (There (Here Owl))) :: Animal
-- -}
-- @
data SumType (types :: [Type]) where
  Here  :: forall x xs. x          -> SumType (x ': xs)
  There :: forall x xs. SumType xs -> SumType (x ': xs)

deriving instance (Show x, Show (SumType xs)) => Show (SumType (x ': xs))
instance Show (SumType '[]) where
  show = error "impossible"

deriving instance (Eq x, Eq (SumType xs)) => Eq (SumType (x ': xs))
instance Eq (SumType '[]) where
  (==) = error "impossible"

deriving instance (Ord x, Ord (SumType xs)) => Ord (SumType (x ': xs))
instance Ord (SumType '[]) where
  compare = error "impossible"

instance (FromJSON x, FromJSON (SumType xs)) => FromJSON (SumType (x ': xs)) where
  parseJSON v = (Here <$> parseJSON v) <|> (There <$> parseJSON v)

instance FromJSON (SumType '[]) where
  parseJSON _ = fail "Could not parse sum type"
