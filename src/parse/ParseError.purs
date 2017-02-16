module Nulan.ParseError where

import Prelude
import Nulan.Source (Source')


data ParseError
  = TabsNotAllowed Source'
  | MissingEndComment Source'
  | MissingStartParen String Source'
  | MissingEndParen String Source'
  | MissingLeftExpression String Source'
  | MissingRightExpression String Source'
  | LambdaMissingBody String Source'


printError :: String -> Source' -> String
printError message source = "Error: " <> message <> "  " <> show source


instance showParseError :: Show ParseError where
  show (TabsNotAllowed source)                = printError "tabs are not allowed" source
  show (MissingEndComment source)             = printError "missing ending /#" source
  show (MissingStartParen start source)       = printError ("missing starting " <> start) source
  show (MissingEndParen end source)           = printError ("missing ending " <> end) source
  show (MissingLeftExpression middle source)  = printError ("missing expression on the left side of " <> middle) source
  show (MissingRightExpression middle source) = printError ("missing expression on the right side of " <> middle) source
  show (LambdaMissingBody name source)        = printError ("missing expression on the right side of " <> name) source
