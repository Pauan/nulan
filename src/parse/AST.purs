module Nulan.AST where

import Prelude
import Data.Foldable (intercalate)
import Nulan.Source (Source)


data AST'
  = Wildcard

  | Integer String
  | Number String
  | Text String
  | Symbol String

  | Lambda (Array AST) AST

  | Parens (Array AST)
  | Array (Array AST)
  | Record (Array AST)

  | Quote AST
  | Unquote AST
  | Splice AST

  | Dot AST AST
  | Match AST AST
  | Assign AST AST
  | Type AST AST

derive instance eqAST' :: Eq AST'

-- TODO remove unnecessary parens from the output
instance showAST' :: Show AST' where
  show Wildcard = "_"

  show (Integer a) = a
  show (Number a) = a
  show (Text a) = show a
  show (Symbol a) = a

  -- TODO better spacing when it only has a body
  show (Lambda a b) = "(-> " <> intercalate " " (map show a) <> " " <> show b <> ")"

  show (Parens a) = "(" <> intercalate " " (map show a) <> ")"
  -- TODO better spacing when it is empty
  show (Array a) = "[ " <> intercalate " " (map show a) <> " ]"
  -- TODO better spacing when it is empty
  show (Record a) = "{ " <> intercalate " " (map show a) <> " }"

  show (Quote a) = "&" <> show a
  show (Unquote a) = "~" <> show a
  show (Splice a) = "@" <> show a

  show (Dot a b) = "(" <> show a <> "." <> show b <> ")"
  show (Match a b) = "(" <> show a <> " : " <> show b <> ")"
  show (Assign a b) = "(" <> show a <> " <= " <> show b <> ")"
  show (Type a b) = "(" <> show a <> " :: " <> show b <> ")"

type AST = Source AST'
