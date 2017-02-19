module Nulan.Array where

foreign import interleave :: forall a. a -> Array a -> Array a
