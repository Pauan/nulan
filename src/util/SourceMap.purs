module Nulan.SourceMap where

import Prelude
import Nulan.Source (Source'(..))
import Nulan.Position (Position(..))


foreign import data SourceMap :: *

foreign import fromStringImpl :: String -> Int -> Int -> String -> SourceMap

foreign import fromArrayImpl :: String -> Int -> Int -> Array SourceMap -> SourceMap

foreign import generateImpl :: forall a. (String -> String -> a) -> String -> String -> SourceMap -> a


fromString :: Source' -> String -> SourceMap
fromString (Source' { filename, start: Position start }) a =
  fromStringImpl filename (start.line + 1) start.column a


fromArray :: Source' -> Array SourceMap -> SourceMap
fromArray (Source' { filename, start: Position start }) a =
  fromArrayImpl filename (start.line + 1) start.column a


fromSourceMap :: String -> String -> SourceMap -> { code :: String, map :: String }
fromSourceMap = generateImpl { code: _, map: _ }
