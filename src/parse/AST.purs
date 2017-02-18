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

{-pretty Wildcard = "_"

pretty (Integer a) = a
pretty (Number a) = a
pretty (Text a) = show a
pretty (Symbol a) = a

-- TODO better spacing when it only has a body
pretty (Lambda a b) = "(-> " <> intercalate " " (map pretty a) <> " " <> pretty b <> ")"

pretty (Parens a) = "(" <> intercalate " " (map pretty a) <> ")"
-- TODO better spacing when it is empty
pretty (Array a) = "[ " <> intercalate " " (map pretty a) <> " ]"
-- TODO better spacing when it is empty
pretty (Record a) = "{ " <> intercalate " " (map pretty a) <> " }"

pretty (Quote a) = "&" <> pretty a
pretty (Unquote a) = "~" <> pretty a
pretty (Splice a) = "@" <> pretty a

pretty (Dot a b) = "(" <> pretty a <> "." <> pretty b <> ")"
pretty (Match a b) = "(" <> pretty a <> " : " <> pretty b <> ")"
pretty (Assign a b) = "(" <> pretty a <> " <= " <> pretty b <> ")"
pretty (Type a b) = "(" <> pretty a <> " :: " <> pretty b <> ")"-}

-- TODO remove unnecessary parens from the output
instance showAST' :: Show AST' where
  show Wildcard = "Wildcard"

  show (Integer a) = "(Integer " <> show a <> ")"
  show (Number a) = "(Number " <> show a <> ")"
  show (Text a) = "(Text " <> show a <> ")"
  show (Symbol a) = "(Symbol " <> show a <> ")"

  show (Lambda a b) = "(Lambda " <> show a <> " " <> show b <> ")"

  show (Parens a) = "(Parens" <> show a <> ")"
  -- TODO better spacing when it is empty
  show (Array a) = "(Array " <> show a <> " ]"
  -- TODO better spacing when it is empty
  show (Record a) = "(Record " <> show a <> " }"

  show (Quote a) = "(Quote " <> show a <> ")"
  show (Unquote a) = "(Unquote " <> show a <> ")"
  show (Splice a) = "(Splice " <> show a <> ")"

  show (Dot a b) = "(Dot " <> show a <> " " <> show b <> ")"
  show (Match a b) = "(Match " <> show a <> " " <> show b <> ")"
  show (Assign a b) = "(Assign " <> show a <> " " <> show b <> ")"
  show (Type a b) = "(Type " <> show a <> " " <> show b <> ")"

type AST = Source AST'
