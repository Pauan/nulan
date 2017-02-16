module Nulan.AST where

import Prelude
import Nulan.Source (Source)


data AST'
  = Integer String
  | Number String
  | Text String
  | Symbol String
  | Parens (Array AST)
  | Array (Array AST)
  | Record (Array AST)
  | Assign AST AST
  | Type AST AST

derive instance eqAST' :: Eq AST'

instance showAST' :: Show AST' where
  show (Integer a) = "(Integer " <> a <> ")"
  show (Number a) = "(Number " <> a <> ")"
  show (Text a) = "(Text " <> show a <> ")"
  show (Symbol a) = "(Symbol " <> show a <> ")"
  show (Parens a) = "(Parens " <> show a <> ")"
  show (Array a) = "(Array " <> show a <> ")"
  show (Record a) = "(Record " <> show a <> ")"
  show (Assign a b) = "(Assign " <> show a <> " " <> show b <> ")"
  show (Type a b) = "(Type " <> show a <> " " <> show b <> ")"

type AST = Source AST'
