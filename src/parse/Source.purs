module Nulan.Source where

import Prelude
import Nulan.Position (Position)


newtype Source =
  Source { filename :: String
         , start :: Position
         , end :: Position }


instance semigroupSource :: Semigroup Source where
  append (Source left) (Source right) =
    Source { filename: right.filename
           , start: left.start
           , end: right.end }
