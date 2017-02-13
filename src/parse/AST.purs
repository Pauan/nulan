module Nulan.AST where

import Prelude
import Nulan.Source (Source)


data AST
  = Integer String Source
  | Number String Source
  | Text String Source
  | Symbol String Source
  | Parens (Array AST) Source
  | Array (Array AST) Source
  | Record (Array AST) Source
  | Assign AST AST Source
  | Type AST AST Source
