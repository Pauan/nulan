module Nulan.Compile.JavaScript where

import Prelude
import Nulan.Array (interleave)
import Nulan.AST (VariableId)
import Nulan.Source (Source(..))
import Nulan.SourceMap (SourceMap, fromArray, fromString, fromSourceMap)
import Nulan.Compile (Statement, Statement'(..), Expression, Expression'(..), RecordPair, RecordPair'(..))


printVariableId :: VariableId -> String
printVariableId a = "_" <> show a


generateVariableId :: Source VariableId -> SourceMap
generateVariableId (Source a source) = fromString source $ printVariableId a


generateKey :: Source String -> SourceMap
generateKey (Source a source) =
  fromString source a


generatePair :: RecordPair -> SourceMap
generatePair (Source (RecordPair' { key, value }) source) =
  fromArray source $
    [ generateKey key
    , fromString source ": "
    , generateExpression value ]


generateExpression :: Expression -> SourceMap
generateExpression (Source (Variable id) source) =
  fromString source $ printVariableId id

generateExpression (Source (Int32 a) source) =
  fromString source $ show a

generateExpression (Source (Float64 a) source) =
  fromString source $ show a

generateExpression (Source (String a) source) =
  fromString source $ show a

generateExpression (Source (Function args body) source) =
  fromArray source $
    [ fromString source "function (" ] <>
    (interleave (fromString source ", ") (map generateVariableId args)) <>
    [ fromString source ") { "
    , generateStatement body
    , fromString source " }" ]

generateExpression (Source (FunctionCall expr args) source) =
  fromArray source $
    [ generateExpression expr
    , fromString source "(" ] <>
    (interleave (fromString source ", ") (map generateExpression args)) <>
    [ fromString source ")" ]

generateExpression (Source (Record a) source) =
  fromArray source $
    [ fromString source "{ " ] <>
    (interleave (fromString source ", ") (map generatePair a)) <>
    [ fromString source " }" ]

generateExpression (Source (RecordGet a key) source) =
  fromArray source $
    [ generateExpression a
    , fromString source "."
    , generateKey key ]

generateExpression (Source (IsEqual a b) source) =
  fromArray source $
    [ generateExpression a
    , fromString source " === "
    , generateExpression b ]


generateStatement :: Statement -> SourceMap
generateStatement (Source (FunctionReturn expr) source) =
  fromArray source $
    [ fromString source "return "
    , generateExpression expr
    , fromString source ";" ]

generateStatement (Source (If expr yes no) source) =
  fromArray source $
    [ fromString source "if ("
    , generateExpression expr
    , fromString source ") { "
    , generateStatement yes
    , fromString source " } else { "
    , generateStatement no
    , fromString source " }" ]

generateStatement (Source (Constant var expr) source) =
  fromArray source $
    [ fromString source "const "
    , generateVariableId var
    , fromString source " = "
    , generateExpression expr
    , fromString source ";" ]

generateStatement (Source (Statements a) source) =
  fromArray source $
    interleave (fromString source " ") (map generateStatement a)

generateStatement (Source (Recursive a) source) =
  fromArray source $
    interleave (fromString source " ") (map generateStatement a)

generateStatement (Source Noop source) =
  fromString source ";"


generate :: String -> String -> Statement -> { code :: String, map :: String }
generate filename map a = fromSourceMap filename map $ generateStatement a
