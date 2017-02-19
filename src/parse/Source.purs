module Nulan.Source where

import Prelude
import Nulan.Position (Position(..))
import Nulan.Pretty (class Pretty, pretty)


newtype Source' =
  Source' { filename :: String
          , start :: Position
          , end :: Position }

derive instance eqSource' :: Eq Source'

instance showSource' :: Show Source' where
  show (Source' { filename, start, end }) =
    "(Source' { filename: " <> show filename <> ", start: " <> show start <> ", end: " <> show end <> " })"


instance semigroupSource' :: Semigroup Source' where
  append (Source' left) (Source' right) =
    Source' { filename: right.filename
            , start: left.start
            , end: right.end }


data Source a =
  Source a Source'

derive instance eqSource :: Eq a => Eq (Source a)

instance functorSource :: Functor Source where
  map f (Source a b) = Source (f a) b

instance showSource :: Show a => Show (Source a) where
  show (Source a b) = "(Source " <> show a <> " " <> show b <> ")"

instance prettySource :: Pretty a => Pretty (Source a) where
  pretty (Source a _) = pretty a


getValue :: forall a. Source a -> a
getValue (Source a _) = a

source :: forall a. a -> String -> Position -> Position -> Source a
source a filename start end = Source a $ Source' { filename, start, end }

getSource :: forall a. Source a -> Source'
getSource (Source _ a) = a
