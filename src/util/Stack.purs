module Nulan.Stack where

import Data.Maybe (Maybe(..))
import Data.List (List(..))


type Stack = List


empty :: forall a. Stack a
empty = Nil


peek :: forall a. Stack a -> Maybe a
peek (Cons a _) = Just a
peek Nil = Nothing


push :: forall a. Stack a -> a -> Stack a
push a b = Cons b a


pop :: forall a. Stack a -> Stack a
pop (Cons _ a) = a
-- TODO maybe throw an error in this case ?
pop Nil = Nil


isEmpty :: forall a. Stack a -> Boolean
isEmpty Nil = true
isEmpty _ = false
