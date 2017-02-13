module Nulan.Position where

import Prelude


newtype Position =
  Position { index :: Int
           , line :: Int
           , column :: Int }


incrementLine :: Position -> Position
incrementLine (Position { index, line }) =
  Position { index: index + 1
           , line: line + 1
           , column: 0 }


incrementColumn :: Position -> Position
incrementColumn (Position { index, line, column }) =
  Position { index: index + 1
           , line: line
           , column: column + 1 }
