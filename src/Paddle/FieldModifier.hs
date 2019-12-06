module Paddle.FieldModifier
  ( modifier
  , customJSONOptions
  ) where

import Data.Aeson
import Protolude
import Prelude ()
import           Data.Char                      (toLower, isUpper)

modifier :: [Char] -> [Char]
modifier = concatMap (\x -> if isUpper x then ['_', toLower x] else [x])

customJSONOptions :: Options
customJSONOptions = defaultOptions
    { fieldLabelModifier = modifier
    }