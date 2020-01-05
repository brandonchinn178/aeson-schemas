{-# LANGUAGE TemplateHaskell #-}

module Enums (State(..), Color(..)) where

import Data.Aeson.Schema.TH (genFromJSONEnum, genToJSONEnum, mkEnum)

mkEnum "State" ["OPEN", "CLOSED"]

data Color = Red | LightBlue | Yellow | DarkGreen | Black | JustABitOffWhite
  deriving (Show, Eq, Enum)

genFromJSONEnum ''Color
genToJSONEnum ''Color
