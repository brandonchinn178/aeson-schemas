{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Schema.Utils.NameLike
  ( NameLike(..)
  , resolveName
  ) where

import Data.Text (Text)
import Language.Haskell.TH.Syntax (Name, Q, lookupTypeName, nameBase)

data NameLike = NameRef String | NameTH Name

instance Eq NameLike where
  ty1 == ty2 = show ty1 == show ty2

instance Show NameLike where
  show (NameRef ty) = ty
  show (NameTH ty) = nameBase ty

resolveName :: NameLike -> Q Name
resolveName = \case
  -- some hardcoded cases
  NameRef "Bool"   -> pure ''Bool
  NameRef "Int"    -> pure ''Int
  NameRef "Double" -> pure ''Double
  NameRef "Text"   -> pure ''Text

  -- general cases
  NameRef name     -> lookupTypeName name >>= maybe (fail $ "Unknown type: " ++ name) pure
  NameTH name      -> pure name
