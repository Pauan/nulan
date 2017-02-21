module Nulan.ParseError where

import Prelude
import Nulan.AST (AST')
import Nulan.Source (Source'(..))
import Nulan.Position (Position(..))
import Nulan.Pretty (pretty)


data ParseError
  = InternalError Source'
  | TabsNotAllowed Source'
  | MissingEndComment Source'
  | MissingStartParen String Source'
  | MissingEndParen String Source'
  | MissingLeftExpression String Source'
  | MissingRightExpression String Source'
  | LambdaMissingBody String Source'
  | ExpectedStatement AST' Source'
  | ExpectedSymbol AST' Source'
  | ExpectedExpression AST' Source'
  | EmptyParens AST' Source'
  | InvalidInt32 String Source'
  | VariableDefined String Source'
  | VariableSeen String Source'
  | VariableUndefined String Source'
  | PatternMatchFailed Source'


printError :: String -> Source' -> String
printError message (Source' { filename, start: Position start }) =
  "Error: " <> message <> "  (" <> filename <> " " <> show (start.line + 1) <> ":" <> show (start.column + 1) <> ")"


instance showParseError :: Show ParseError where
  show (InternalError source)                 = printError "internal error: this should never happen" source
  show (TabsNotAllowed source)                = printError "tabs are not allowed" source
  show (MissingEndComment source)             = printError "missing ending /#" source
  show (MissingStartParen start source)       = printError ("missing starting " <> start) source
  show (MissingEndParen end source)           = printError ("missing ending " <> end) source
  show (MissingLeftExpression middle source)  = printError ("missing expression on the left side of " <> middle) source
  show (MissingRightExpression middle source) = printError ("missing expression on the right side of " <> middle) source
  show (LambdaMissingBody name source)        = printError ("missing expression on the right side of " <> name) source
  show (ExpectedStatement ast source)         = printError ("expected statement but got " <> pretty ast) source
  show (ExpectedExpression ast source)        = printError ("expected expression but got " <> pretty ast) source
  show (ExpectedSymbol a source)              = printError ("expected symbol but got " <> pretty a) source
  show (EmptyParens a source)                 = printError (pretty a <> " is invalid") source
  show (InvalidInt32 a source)                = printError ("invalid Int32: " <> a) source
  show (VariableDefined a source)             = printError ("variable is already defined: " <> a) source
  show (VariableSeen a source)                = printError ("variable is already defined: " <> a) source
  show (VariableUndefined a source)           = printError ("variable is not defined: " <> a) source
  show (PatternMatchFailed source)            = printError ("none of the patterns matched") source
