module Nulan.Source where

import Prelude
import Nulan.Position (Position(..))


newtype Source' =
  Source' { filename :: String
         , start :: Position
         , end :: Position }

derive instance eqSource' :: Eq Source'

instance showSource' :: Show Source' where
  show (Source' { filename, start: Position start }) =
    "(" <> filename <> " " <> show (start.line + 1) <> ":" <> show (start.column + 1) <> ")"


instance semigroupSource' :: Semigroup Source' where
  append (Source' left) (Source' right) =
    Source' { filename: right.filename
            , start: left.start
            , end: right.end }


data Source a =
  Source a Source'

instance functorSource :: Functor Source where
  map f (Source a b) = Source (f a) b

instance showSource :: Show a => Show (Source a) where
  show (Source a _) = show a

instance eqSource :: Eq a => Eq (Source a) where
  eq (Source a _) (Source b _) = eq a b

source :: forall a. Source a -> Source'
source (Source _ a) = a
