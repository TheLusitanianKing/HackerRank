{-# LANGUAGE OverloadedStrings #-}

import Data.Map  (Map)
import Data.Text (Text)

import qualified Data.Map  as Map
import qualified Data.Text as Text

newtype Key = Key Text

data Value = Number Integer
  | RationalNumber Integer Integer
  | Function Int [Value]

newtype Memory = Memory { memory :: Map Key Value }