module Nulan.AST where

import Prelude
import Data.Foldable (all, intercalate)
import Data.Array (null, length)
import Nulan.Array (interleave)
import Nulan.Source (Source, getValue)
import Nulan.Pretty (class Pretty, pretty, indentLine, indentNewline)


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

  | Bar AST
  | Quote AST
  | Unquote AST
  | Splice AST

  | Dot AST AST
  | Match AST AST
  | Assign AST AST
  | Type AST AST

type AST = Source AST'

derive instance eqAST' :: Eq AST'


isSimple :: AST' -> Boolean
isSimple Wildcard = true
isSimple (Integer _) = true
isSimple (Number _) = true
isSimple (Text _) = false
isSimple (Symbol _) = true -- TODO exclude newlines

isSimple (Lambda a b) = all (getValue >>> isSimple) a && isSimple (getValue b)
isSimple (Parens a) = all (getValue >>> isSimple) a
isSimple (Array a) = all (getValue >>> isSimple) a
isSimple (Record a) = all (getValue >>> isSimple) a

isSimple (Bar a) = false
isSimple (Quote a) = isSimple (getValue a)
isSimple (Unquote a) = isSimple (getValue a)
isSimple (Splice a) = isSimple (getValue a)

isSimple (Dot a b) = isSimple (getValue a) && isSimple (getValue b)
isSimple (Match a b) = false
isSimple (Assign a b) = false
isSimple (Type a b) = false


-- TODO is this correct ?
shouldParen :: AST' -> Boolean
shouldParen (Lambda _ _) = true
shouldParen (Dot _ _) = true
shouldParen (Match _ _) = true
shouldParen (Assign _ _) = true
shouldParen (Type _ _) = true
shouldParen _ = false


prettyParen :: AST -> String
prettyParen a =
  if shouldParen (getValue a)
  then "(" <> pretty a <> ")"
  else pretty a


prettyArray' :: (String -> String) -> Array AST -> Array String
prettyArray' f a =
  case length a of
    0 -> []
    1 -> map (\a -> f (pretty a)) a
    _ -> map (\a -> f (prettyParen a)) a


indentPrettyArray :: String -> Array AST -> String
indentPrettyArray inner a =
  intercalate inner (prettyArray' id a)


prettyArray :: String -> Array AST -> String
prettyArray inner a =
  indentLine (interleave inner (prettyArray' id a))


instance prettyAST' :: Pretty AST' where
  pretty Wildcard = "_"

  pretty (Integer a) = a
  pretty (Number a) = a
  pretty (Text a) = show a
  pretty (Symbol a) = a

  pretty (Lambda a b) =
    if null a
    -- TODO should this use prettyParen ?
    then indentLine ["-> ", pretty b]
    else if isSimple (Lambda a b)
    then indentLine ["-> ", prettyArray " " a, " ", prettyParen b]
    else "->\n  " <> indentPrettyArray "\n  " a <> "\n  " <> indentNewline "  " (prettyParen b)

  pretty (Parens a) =
    if isSimple (Parens a)
    then indentLine ["(", prettyArray " " a, ")"]
    -- TODO should this use 2 spaces or 1 ?
    else "(" <> indentPrettyArray "\n  " a <> ")"

  pretty (Array a) =
    if null a
    then "[]"
    else if isSimple (Array a)
    then indentLine ["[ ", prettyArray " " a, " ]"]
    else "[ " <> indentPrettyArray "\n  " a <> " ]"

  pretty (Record a) =
    if null a
    then "{}"
    else if isSimple (Record a)
    then indentLine ["{ ", prettyArray " " a, " }"]
    else "{ " <> indentPrettyArray "\n  " a <> " }"

  pretty (Bar a) = indentLine ["| ", prettyParen a]
  pretty (Quote a) = indentLine ["&", prettyParen a]
  pretty (Unquote a) = indentLine ["~", prettyParen a]
  pretty (Splice a) = indentLine ["@", prettyParen a]

  pretty (Dot a b) = indentLine [prettyParen a, ".", prettyParen b]
  pretty (Match a b) = indentLine [prettyParen a, "\n: ", prettyParen b]
  pretty (Assign a b) = indentLine [prettyParen a, " <= ", prettyParen b]
  pretty (Type a b) = indentLine [prettyParen a, " :: ", prettyParen b]


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

  show (Bar a) = "(Bar " <> show a <> ")"
  show (Quote a) = "(Quote " <> show a <> ")"
  show (Unquote a) = "(Unquote " <> show a <> ")"
  show (Splice a) = "(Splice " <> show a <> ")"

  show (Dot a b) = "(Dot " <> show a <> " " <> show b <> ")"
  show (Match a b) = "(Match " <> show a <> " " <> show b <> ")"
  show (Assign a b) = "(Assign " <> show a <> " " <> show b <> ")"
  show (Type a b) = "(Type " <> show a <> " " <> show b <> ")"
