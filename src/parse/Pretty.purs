module Nulan.Pretty where


class Pretty a where
  pretty :: a -> String


foreign import indentLine :: Array String -> String

foreign import indentNewline :: String -> String -> String
