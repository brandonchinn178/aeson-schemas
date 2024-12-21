{-# LANGUAGE CPP #-}

module Data.Aeson.Schema.Utils.Compat (
  -- * Key
  Key,
  keyToText,

  -- * KeyMap
  KeyMap,
  KeyMap.singleton,
  KeyMap.fromList,
  KeyMap.lookup,
  unions,
) where

#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif

import Data.Text (Text)
import Prelude hiding (lookup)

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap

keyToText :: Key -> Text
keyToText = Key.toText
#else
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as KeyMap

type Key = Text
type KeyMap = HashMap Key

keyToText :: Key -> Text
keyToText = id
#endif

unions :: [KeyMap v] -> KeyMap v
unions = foldl' KeyMap.union KeyMap.empty
