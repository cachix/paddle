module Paddle.Amount 
  ( Amount(..)
  ) where

import Protolude hiding (Show)
import Prelude (Show(..), read)
import Data.Scientific
import Data.Aeson.Types (Parser)
import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, withText, withScientific, Value(Number))

newtype Amount = Amount { getScientific :: Scientific }

instance Show Amount where
    show = formatScientific Fixed Nothing . getScientific

instance Eq Amount where
    a1 == a2 = getScientific a1 == getScientific a2

instance FromJSON Amount where
    parseJSON value =
          withText "amount" f value
      <|> withScientific "amount" (pure . Amount) value
      where
        f :: Text -> Parser Amount
        f t = pure $ Amount (read (toS t))

instance ToJSON Amount where
    toJSON = Number . getScientific