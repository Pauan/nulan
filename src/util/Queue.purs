module Nulan.Queue where

import Prelude
import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl, foldr, foldrDefault, foldMapDefaultL)


data Queue a = Queue (List a) (List a)


empty :: forall a. Queue a
empty = Queue Nil Nil


singleton :: forall a. a -> Queue a
singleton a = Queue (Cons a Nil) Nil


isEmpty :: forall a. Queue a -> Boolean
isEmpty (Queue Nil _) = true
isEmpty _             = false


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


{-
1 2 3 -- 6 5 4

--          -> --
1 --        -> --        (1)
1 -- 2      -> 1 --      (2)
1 -- 3 2    -> 1 -- 2    (3)
1 2 --      -> 1 --      (2)
1 2 -- 3    -> 1 2 --    (3)
1 2 -- 4 3  -> 1 2 -- 3  (4)
1 2 3 4 --  -> 1 -- 3 2  (4)
-}
unsnoc :: forall a. Queue a -> Maybe (Tuple a (Queue a))
unsnoc (Queue Nil          _)          = Nothing
unsnoc (Queue l            (Cons v r)) = Just (Tuple v (Queue l r))
unsnoc (Queue (Cons v Nil) Nil)        = Just (Tuple v empty)
unsnoc (Queue (Cons v l)   Nil)        = unsnoc (Queue (Cons v Nil) (reverse l)) -- TODO is this correct ?


snoc :: forall a. Queue a -> a -> Queue a
snoc (Queue Nil r) v = Queue (Cons v Nil) r
snoc (Queue l   r) v = Queue l            (Cons v r)


-- TODO make this faster ?
instance semigroupQueue :: Semigroup (Queue a) where
  append (Queue Nil _) r = r
  append l (Queue Nil _) = l
  append l r = foldl snoc l r


foreign import fromStringImpl :: (Queue Char -> Char -> Queue Char) -> Queue Char -> String -> Queue Char

fromString :: String -> Queue Char
fromString = fromStringImpl snoc empty


instance showQueue :: (Show a) => Show (Queue a) where
  show (Queue Nil Nil) = "(Queue)"
  show q = "(Queue " <> (foldl (\a b -> a <> "\n  " <> show b) "" q) <> "\n)"
