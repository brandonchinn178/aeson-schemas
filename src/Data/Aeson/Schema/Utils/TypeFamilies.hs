{-|
Module      :  Data.Aeson.Schema.Utils.TypeFamilies
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Utilities for working with type families.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aeson.Schema.Utils.TypeFamilies
  ( All
  ) where

import Data.Kind (Constraint)

type family All (f :: a -> Constraint) (xs :: [a]) :: Constraint where
  All _ '[] = ()
  All f (x ': xs) = (f x, All f xs)
