module Nulan.Number where

import Data.Maybe (Maybe(..))

foreign import fromStringImpl :: (Number -> Maybe Number) -> Maybe Number -> String -> Maybe Number

fromString :: String -> Maybe Number
fromString = fromStringImpl Just Nothing
