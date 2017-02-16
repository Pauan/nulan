module Nulan.Queue where

import Prelude
import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl, foldr, foldrDefault, foldMapDefaultL)


data Queue a = Queue (List a) (List a)


empty :: forall a. Queue a
empty = Queue Nil Nil


instance foldableQueue :: Foldable Queue where
  foldl f init (Queue l r) = foldr (flip f) (foldl f init l) r
  foldr f init (Queue l r) = foldr f (foldl (flip f) init r) l

  -- TODO is this correct ?
  foldMap = foldMapDefaultL
  --foldr a = foldrDefault a


uncons :: forall a. Queue a -> Maybe (Tuple a (Queue a))
uncons (Queue Nil          _) = Nothing
uncons (Queue (Cons v Nil) r) = Just (Tuple v (Queue (reverse r) Nil))
uncons (Queue (Cons v l)   r) = Just (Tuple v (Queue l           r))


snoc :: forall a. Queue a -> a -> Queue a
snoc (Queue Nil r) v = Queue (Cons v Nil) r
snoc (Queue l   r) v = Queue l            (Cons v r)


foreign import fromStringImpl :: (Queue Char -> Char -> Queue Char) -> Queue Char -> String -> Queue Char

fromString :: String -> Queue Char
fromString = fromStringImpl snoc empty


instance showQueue :: (Show a) => Show (Queue a) where
  show (Queue Nil Nil) = "(Queue)"
  show q = "(Queue " <> (foldl (\a b -> a <> "\n  " <> show b) "" q) <> "\n)"
