module Nulan.Compile where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Traversable (traverse)
import Data.Array as Array
import Data.Int (fromString)
import Data.Map as Map

import Nulan.ParseError (ParseError(..))
import Nulan.AST as AST
import Nulan.Source (Source(..), Source'(..))
import Nulan.Position (position)
import Nulan.Queue (Queue, empty, snoc)
import Nulan.Stack as Stack
import Nulan.State (State, runState, getState, putState, modifyState, throwError)


newtype VariableInfo = VariableInfo
  { id :: AST.VariableId
  , name :: String
  , source :: Source'
  , macro :: Maybe (AST.AST -> Compiler AST.AST)
  , statement :: Maybe (AST.AST -> Compiler Statement)
  , expression :: Maybe (AST.AST -> Compiler Expression) }

newtype Scope = Scope
  { vars :: Map.Map AST.VariableId VariableInfo
  , defined :: Map.Map String AST.VariableId
  , seen :: Map.Map String AST.VariableId }


type CompileState =
  { varCounter :: Int
  , scopes :: Stack.Stack Scope
  , statements :: Queue Statement }

type Compiler = State ParseError CompileState


getStatements :: Compiler (Queue Statement)
getStatements = do
  { statements } <- getState
  pure statements


setStatements :: Queue Statement -> Compiler Unit
setStatements statements = modifyState \{ varCounter, scopes } ->
  { varCounter
  , scopes
  , statements }


pushStatement :: Statement -> Compiler Unit
pushStatement statement = modifyState \{ varCounter, scopes, statements } ->
  { varCounter
  , scopes
  , statements: snoc statements statement }


withStatements :: Compiler Statement -> Compiler Statement
withStatements run = do
  old <- getStatements
  setStatements empty
  (Source output source) <- run
  statements <- getStatements
  setStatements old
  case Array.fromFoldable (snoc statements $ Source output source) of
    [statement] -> pure $ statement
    -- TODO is this source correct ?
    statements -> pure $ Source (Statements statements) source


pushScope :: Compiler Unit
pushScope = modifyState \{ varCounter, scopes, statements } ->
  { varCounter
  , scopes: Stack.push scopes $ Scope { vars: Map.empty
                                      , defined: Map.empty
                                      , seen: Map.empty }
  , statements }


popScope :: Compiler Unit
popScope = modifyState \{ varCounter, scopes, statements } ->
  { varCounter
  , scopes: Stack.pop scopes
  , statements }


withNewScope :: forall a. Compiler a -> Compiler a
withNewScope run = do
  pushScope
  output <- run
  popScope
  pure $ output


lookupScope :: forall a. Stack.Stack Scope -> (Scope -> Maybe a) -> Maybe a
lookupScope scopes fn = do
  scope <- Stack.peek scopes
  case fn scope of
    Just a -> Just a
    Nothing -> lookupScope (Stack.pop scopes) fn


lookupSymbol' :: String -> Compiler (Maybe VariableInfo)
lookupSymbol' name = do
  { scopes } <- getState
  pure $ lookupScope scopes \(Scope scope) -> do
    id <- Map.lookup name scope.defined
    -- TODO throw an error if this fails ?
    Map.lookup id scope.vars


lookupVariable' :: AST.VariableId -> Compiler (Maybe VariableInfo)
lookupVariable' id = do
  { scopes } <- getState
  pure $ lookupScope scopes \(Scope scope) ->
    Map.lookup id scope.vars


compileSymbol :: String -> Source' -> Compiler Expression
compileSymbol name source = do
  var <- lookupSymbol' name
  case var of
    Just (VariableInfo { id }) ->
      pure $ Source (Variable id) source

    Nothing ->
      throwError $ VariableUndefined name source


lookupSymbol :: AST.AST' -> Compiler (Maybe VariableInfo)
lookupSymbol (AST.Symbol name) = lookupSymbol' name
lookupSymbol (AST.Variable id) = lookupVariable' id
lookupSymbol _ = pure $ Nothing


newVariableId :: Compiler AST.VariableId
newVariableId = do
  { varCounter } <- getState
  modifyState \{ varCounter, scopes, statements } ->
    { varCounter: varCounter + 1
    , scopes
    , statements }
  pure varCounter


defineNewVariable :: VariableInfo -> Compiler Unit
defineNewVariable info@(VariableInfo { id, name, source }) = do
  { scopes } <- getState

  case Stack.peek scopes of
    Just (Scope scope) ->
      if Map.member name scope.defined
      then throwError $ VariableDefined name source

      else if Map.member name scope.seen
      then throwError $ VariableSeen name source

      else if Map.member id scope.vars
      then throwError $ InternalError source

      else
        modifyState \{ varCounter, scopes, statements } ->
          { varCounter
          -- TODO a tiny bit hacky
          , scopes: Stack.push (Stack.pop scopes)
              (Scope { vars: Map.insert id info scope.vars
                     , defined: Map.insert name id scope.defined
                     , seen: scope.seen })
          , statements }

    Nothing ->
      throwError $ InternalError source


newtype RecordPair =
  RecordPair { key :: String, value :: Expression }

derive instance eqRecordPair :: Eq RecordPair

instance showRecordPair :: Show RecordPair where
  show (RecordPair a) = "{ key: " <> a.key <> ", value: " <> show a.value <> " }"


data Expression'
  = Variable AST.VariableId
  | Int32 Int
  | Float64 Number
  | String String

  | Function (Array (Source AST.VariableId)) Statement
  | FunctionCall Expression (Array Expression)

  | Array (Array Expression)

  | Record (Array RecordPair)
  | RecordGet Expression String

  | IsEqual Expression Expression

type Expression = Source Expression'

derive instance eqExpression' :: Eq Expression'


instance showExpression' :: Show Expression' where
  show (Variable a) = "(Variable " <> show a <> ")"
  show (Int32 a) = "(Int32 " <> show a <> ")"
  show (Float64 a) = "(Float64 " <> show a <> ")"
  show (String a) = "(String " <> show a <> ")"
  show (Function args body) = "(Function " <> show args <> " " <> show body <> ")"
  show (FunctionCall expr args) = "(FunctionCall " <> show expr <> " " <> show args <> ")"
  show (Array a) = "(Array " <> show a <> ")"
  show (Record a) = "(Record " <> show a <> ")"
  show (RecordGet a b) = "(RecordGet " <> show a <> " " <> show b <> ")"
  show (IsEqual a b) = "(IsEqual " <> show a <> " " <> show b <> ")"


data Statement'
  = FunctionReturn Expression
  | If Expression Statement Statement
  | Constant (Source AST.VariableId) Expression
  | Statements (Array Statement)
  | Recursive (Array Statement)
  | Noop

type Statement = Source Statement'

derive instance eqStatement' :: Eq Statement'


instance showStatement' :: Show Statement' where
  show (FunctionReturn expr) = "(FunctionReturn " <> show expr <> ")"
  show (If expr yes no) = "(If " <> show expr <> " " <> show yes <> " " <> show no <> ")"
  show (Constant var expr) = "(Constant " <> show var <> " " <> show expr <> ")"
  show (Statements a) = "(Statements " <> show a <> ")"
  show (Recursive a) = "(Recursive " <> show a <> ")"
  show Noop = "Noop"


compileExpression :: AST.AST -> Compiler Expression
compileExpression (Source (AST.Symbol var) source) =
  compileSymbol var source

compileExpression (Source (AST.Integer a) source) =
  case fromString a of
    Just a ->
      pure $ Source (Int32 a) source

    Nothing ->
      throwError $ InvalidInt32 a source

compileExpression (Source (AST.Lambda args body) source) =
  withNewScope do
    args <- traverse compileNewVariable args
    body <- withStatements do
      (Source body source) <- compileExpression body
      pure $ Source (FunctionReturn $ Source body source) source
    pure $ Source (Function args body) source

compileExpression (Source (AST.Parens a) source) =
  case Array.uncons a of
    Just { head, tail } -> do
      head <- compileExpression head
      tail <- traverse compileExpression tail
      pure $ Source (FunctionCall head tail) source

    Nothing ->
      throwError $ EmptyParens (AST.Parens a) source

compileExpression (Source expr source) =
  throwError $ ExpectedExpression expr source


makeEmptyVariable :: String -> Source' -> Compiler VariableInfo
makeEmptyVariable name source = do
  id <- newVariableId
  pure $ VariableInfo
    { id
    , name
    , source
    , macro: Nothing
    , statement: Nothing
    , expression: Nothing }


compileNewVariable :: AST.AST -> Compiler (Source AST.VariableId)
compileNewVariable (Source (AST.Symbol var) source) = do
  info@(VariableInfo { id }) <- makeEmptyVariable var source
  defineNewVariable info
  pure $ Source id source

compileNewVariable (Source a source) =
  throwError $ ExpectedSymbol a source


builtinSource :: Source'
builtinSource = Source'
  { filename: "nulan:builtin"
  , start: position 0 0 0
  , end: position 0 0 0 }


defineBuiltinStatement :: String -> (AST.AST -> Compiler Statement) -> Compiler AST.VariableId
defineBuiltinStatement name fn = do
  id <- newVariableId
  defineNewVariable $ VariableInfo
    { id
    , name
    , source: builtinSource
    , macro: Nothing
    , expression: Nothing
    , statement: Just fn }
  pure id


defineBuiltins :: Compiler Unit
defineBuiltins = do
  constantId <- defineBuiltinStatement "CONSTANT" \(Source ast source) -> do
    case ast of
      AST.Parens [_, (Source (AST.Type var _) _), body] -> do
        body <- compileExpression body
        withNewScope do
          var <- compileNewVariable var
          pure $ Source (Constant var body) source

      _ ->
        throwError $ PatternMatchFailed source


  -- TODO what about nested RECURSIVE ?
  _ <- defineBuiltinStatement "RECURSIVE" \(Source ast source) -> do
    case ast of
      AST.Parens a ->
        case Array.tail a of
          Just rest -> do
            output <- traverse compileStatement rest
            -- TODO is this correct ?
            pure $ Source (Recursive output) source

          Nothing ->
            throwError $ PatternMatchFailed source

      _ ->
        throwError $ PatternMatchFailed source

  pure unit


compileStatement :: AST.AST -> Compiler Statement
compileStatement (Source (AST.Parens a) source) =
  case Array.head a of
    Just (Source first _) -> do
      var <- lookupSymbol first
      case var of
        Just (VariableInfo var) ->
          case var.macro of
            Just macro -> do
              output <- macro $ Source (AST.Parens a) source
              compileStatement output

            Nothing ->
              case var.statement of
                Just statement ->
                  statement $ Source (AST.Parens a) source

                Nothing ->
                  throwError $ ExpectedStatement (AST.Parens a) source

        Nothing ->
          throwError $ ExpectedStatement (AST.Parens a) source

    Nothing ->
      throwError $ EmptyParens (AST.Parens a) source

compileStatement (Source a source) =
  throwError $ ExpectedStatement a source


compile :: Either ParseError (Source (Queue AST.AST)) -> Either ParseError Statement
compile a = do
  (Source a source) <- a
  runState
    (withNewScope do
      defineBuiltins
      output <- traverse compileStatement $ Array.fromFoldable a
      pure $ Source (Statements output) source)
    { varCounter: 0
    , scopes: Stack.empty
    , statements: empty }
