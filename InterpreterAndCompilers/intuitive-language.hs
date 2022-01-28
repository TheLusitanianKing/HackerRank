{-# LANGUAGE OverloadedStrings #-}

import Data.Map  (Map)
import Data.Ratio (Rational)
import Data.Text (Text)

import qualified Data.Map   as Map
import qualified Data.Ratio as Ratio
import qualified Data.Text  as Text

newtype Key = Key Text

createKey :: Text -> Maybe Key
createKey t = undefined

data Value = Number Rational | Function [Value]

newtype Memory = Memory { memory :: Map Key Value }