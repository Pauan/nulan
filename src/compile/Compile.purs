module Nulan.Compile where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Traversable (traverse)
import Data.Array as Array
import Data.Int (fromString)
import Data.Map as Map

import Nulan.Number as Number
import Nulan.ParseError (ParseError(..))
import Nulan.AST as AST
import Nulan.Source (Source(..), Source'(..))
import Nulan.Position (position)
import Nulan.Queue (Queue, empty, snoc)
import Nulan.Stack as Stack
import Nulan.State (State, runState, getState, putState, modifyState, throwError)


data VariableInfo
  = Explicit
      { id :: AST.VariableId
      , name :: String
      , source :: Source'
      , macro :: Maybe (AST.AST -> Compiler AST.AST) }
  | Gensym
      { id :: AST.VariableId
      , source :: Source'
      , macro :: Maybe (AST.AST -> Compiler AST.AST) }
  | Builtin
      { id :: AST.VariableId
      , name :: String
      , source :: Source'
      , statement :: Maybe (AST.AST -> Compiler Statement)
      , expression :: Maybe (AST.AST -> Compiler Expression) }


getVariableId :: VariableInfo -> AST.VariableId
getVariableId (Explicit { id }) = id
getVariableId (Builtin { id }) = id
getVariableId (Gensym { id }) = id


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


lookupScope' :: forall a. Stack.Stack Scope -> (Scope -> Maybe a) -> Maybe a
lookupScope' scopes fn = do
  scope <- Stack.peek scopes
  case fn scope of
    Just a -> Just a
    Nothing -> lookupScope' (Stack.pop scopes) fn


lookupScope :: forall a. (Scope -> Maybe a) -> Compiler (Maybe a)
lookupScope fn = do
  { scopes } <- getState
  pure $ lookupScope' scopes fn


lookupSymbol' :: String -> Compiler (Maybe VariableInfo)
lookupSymbol' name = lookupScope \(Scope scope) -> do
  id <- Map.lookup name scope.defined
  -- TODO throw an error if this fails ?
  Map.lookup id scope.vars


lookupVariable' :: AST.VariableId -> Compiler (Maybe VariableInfo)
lookupVariable' id = lookupScope \(Scope scope) -> Map.lookup id scope.vars


compileSymbol :: String -> Source' -> Compiler Expression
compileSymbol name source = do
  var <- lookupSymbol' name
  case var of
    Just var ->
      pure $ Source (Variable $ getVariableId var) source

    Nothing ->
      throwError $ VariableUndefined name source


lookupSymbol :: AST.AST' -> Compiler (Maybe VariableInfo)
lookupSymbol (AST.Symbol name) = lookupSymbol' name
lookupSymbol (AST.Variable id) = lookupVariable' id
lookupSymbol _ = pure $ Nothing


lookupVariableId :: AST.AST' -> Source' -> Compiler (Maybe AST.VariableId)
lookupVariableId (AST.Symbol name) source = do
  id <- lookupScope \(Scope scope) -> Map.lookup name scope.defined
  case id of
    Just id -> pure $ Just id
    Nothing -> throwError $ VariableUndefined name source

lookupVariableId (AST.Variable id) _ = pure $ Just id
lookupVariableId _ _ = pure Nothing


assertVariableId :: AST.AST -> AST.VariableId -> Compiler Unit
assertVariableId (Source a source) expected = do
  id <- lookupVariableId a source
  case id of
    Just id ->
      if id == expected
      then pure unit
      else throwError $ ExpectedVariableId expected id source
    Nothing ->
      throwError $ ExpectedSymbol a source


newVariableId :: Compiler AST.VariableId
newVariableId = do
  { varCounter } <- getState
  modifyState \{ varCounter, scopes, statements } ->
    { varCounter: varCounter + 1
    , scopes
    , statements }
  pure varCounter


getCurrentScope :: Compiler Scope
getCurrentScope = do
  { scopes } <- getState

  case Stack.peek scopes of
    Just scope ->
      pure scope

    Nothing ->
      throwError InternalError


setCurrentScope :: Scope -> Compiler Unit
setCurrentScope scope =
  modifyState \{ varCounter, scopes, statements } ->
    { varCounter
    -- TODO a tiny bit hacky
    , scopes: Stack.push (Stack.pop scopes) scope
    , statements }


defineNewVariable' :: AST.VariableId -> String -> Source' -> VariableInfo -> Compiler Unit
defineNewVariable' id name source info = do
  Scope scope <- getCurrentScope

  if Map.member name scope.defined
    then throwError $ VariableDefined name source

    else if Map.member name scope.seen
    then throwError $ VariableSeen name source

    else if Map.member id scope.vars
    then throwError InternalError

    else
      setCurrentScope $
        Scope { vars: Map.insert id info scope.vars
              , defined: Map.insert name id scope.defined
              , seen: scope.seen }


defineNewVariable :: VariableInfo -> Compiler Unit
defineNewVariable info@(Explicit { id, name, source }) =
  defineNewVariable' id name source info

defineNewVariable info@(Builtin { id, name, source }) =
  defineNewVariable' id name source info

defineNewVariable info@(Gensym { id, source }) = do
  Scope scope <- getCurrentScope

  if Map.member id scope.vars
    -- TODO better error
    then throwError InternalError
    else setCurrentScope $
           Scope { vars: Map.insert id info scope.vars
                 , defined: scope.defined
                 , seen: scope.seen }


newtype RecordPair =
  RecordPair { key :: String, value :: Expression }

derive instance eqRecordPair :: Eq RecordPair

instance showRecordPair :: Show RecordPair where
  show (RecordPair a) = "RecordPair { key: " <> a.key <> ", value: " <> show a.value <> " }"


data Expression'
  = Variable AST.VariableId
  | Int32 Int
  | Float64 Number
  | String String

  | Function (Array (Source AST.VariableId)) Statement
  | FunctionCall Expression (Array Expression)

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

-- TODO handle Uint32
compileExpression (Source (AST.Integer a) source) =
  case fromString a of
    Just a ->
      pure $ Source (Int32 a) source

    Nothing ->
      throwError $ InvalidInt32 a source

compileExpression (Source (AST.Number a) source) =
  case Number.fromString a of
    Just a ->
      pure $ Source (Float64 a) source

    Nothing ->
      throwError $ InvalidFloat64 a source

compileExpression (Source (AST.Text a) source) =
  pure $ Source (String a) source

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

compileExpression (Source (AST.Bar _) source) =
  throwError $ InvalidBar source

compileExpression (Source (AST.Array _) source) =
  throwError $ InvalidArray source

compileExpression (Source AST.Wildcard source) =
  throwError $ InvalidWildcard source

compileExpression (Source expr source) =
  throwError $ ExpectedExpression expr source


makeEmptyVariable :: String -> Source' -> Compiler VariableInfo
makeEmptyVariable name source = do
  id <- newVariableId
  pure $ Explicit
    { id
    , name
    , source
    , macro: Nothing }


compileNewVariable :: AST.AST -> Compiler (Source AST.VariableId)
compileNewVariable (Source (AST.Symbol var) source) = do
  info <- makeEmptyVariable var source
  defineNewVariable info
  pure $ Source (getVariableId info) source

compileNewVariable (Source a source) =
  throwError $ ExpectedSymbol a source


builtinSource :: Source'
builtinSource = Source'
  { filename: "nulan:builtin"
  , start: position 0 0 0
  , end: position 0 0 0 }


defineBuiltinStatement' :: String -> AST.VariableId -> (AST.AST -> Compiler Statement) -> Compiler Unit
defineBuiltinStatement' name id fn =
  defineNewVariable $ Builtin
    { id
    , name
    , source: builtinSource
    , expression: Nothing
    , statement: Just fn }


defineBuiltinStatement :: String -> (AST.AST -> Compiler Statement) -> Compiler Unit
defineBuiltinStatement name fn = do
  id <- newVariableId
  defineBuiltinStatement' name id fn


defineBuiltins :: Compiler Unit
defineBuiltins = do
  constantId <- newVariableId
  defineBuiltinStatement' "CONSTANT" constantId \(Source ast source) -> do
    case ast of
      AST.Parens [_, (Source (AST.Type var _) _), body] -> do
        body <- compileExpression body
        var <- compileNewVariable var
        pure $ Source (Constant var body) source

      _ ->
        throwError $ PatternMatchFailed source


  -- TODO what about nested RECURSIVE ?
  defineBuiltinStatement "RECURSIVE" \(Source ast source) -> do
    case ast of
      AST.Parens a ->
        case Array.tail a of
          Just rest -> do
            bodies <- traverse (\(Source ast source) ->
              case ast of
                AST.Parens [head, (Source (AST.Type var _) _), body@(Source (AST.Lambda _ _) _)] -> do
                  assertVariableId head constantId
                  var <- compileNewVariable var
                  pure $ do
                    body <- compileExpression body
                    pure $ Source (Constant var body) source

                _ ->
                  -- TODO better error
                  throwError $ PatternMatchFailed source) rest
            output <- traverse id bodies
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
        Just (Explicit { macro: Just macro }) -> do
          output <- macro $ Source (AST.Parens a) source
          compileStatement output

        Just (Builtin { statement: Just statement }) ->
          statement $ Source (AST.Parens a) source

        _ ->
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
