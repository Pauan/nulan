module Nulan.Stream where

import Prelude
import Data.Array (index)
import Data.String (charAt)


class Index a b | a -> b where
  lookupIndex :: a -> Int -> Maybe b

instance indexString :: Index String Char where
  lookupIndex a b = charAt b a

instance indexArray :: Index (Array a) a where
  lookupIndex = index


class Sequence a b | a -> b where
  peek :: a -> Maybe b
  next :: a -> a


data Cursor a = Cursor a Int

instance sequenceCursor :: (Index a b) => Sequence (Cursor a) b where
  peek (Cursor a b) = lookupIndex a b
  next (Cursor a b) = Cursor a (b + 1)


toCursor :: forall a b. (Index a b) => Cursor a
toCursor a = Cursor a 0
