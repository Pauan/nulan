module Nulan.Parse where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Data.Array as Array

import Nulan.Queue (Queue, uncons, snoc, unsnoc, empty, fromString, singleton)
import Nulan.ParseError (ParseError(..))
import Nulan.AST (AST, AST'(..))
import Nulan.Position (Position(..))
import Nulan.Source (Source(..), Source', source)
import Nulan.Tokenize (Token, Token'(..))
import Nulan.State (State, runState, getState, putState, throwError)

import Debug.Trace


data Associativity
  = Left
  | Right


type Priority = Int

type TokenInfo =
  { priority :: Priority
  , parse :: Token -> Priority -> Queue AST -> Parser (Queue AST) }


type ParserState =
  { input :: Queue Token }

type Parser = State ParseError ParserState


getCurrent :: Parser (Maybe (Tuple Token (Queue Token)))
getCurrent = do
  { input } <- getState
  pure $ uncons input


setCurrent :: Queue Token -> Parser Unit
setCurrent input = putState { input }


tokenToAST :: Token' -> AST'
tokenToAST (TokenInteger a) = Integer a
tokenToAST (TokenText a)    = Text a
tokenToAST (TokenSymbol a)  = Symbol a


parseParen' :: (Array AST -> AST') -> Source' -> String -> Queue AST -> Parser AST
parseParen' make start right output = do
  current <- getCurrent
  case current of
    Just (Tuple token rest) -> do
      case token of
        Source (TokenSymbol s) end ->
          if s == right
          then do
            setCurrent rest
            pure $ Source (make $ Array.fromFoldable output) (start <> end)

          else do
            output <- parse'' bottom output token rest
            parseParen' make start right output

        _ -> do
          output <- parse'' bottom output token rest
          parseParen' make start right output

    Nothing ->
      throwError $ MissingEndParen right start


parseParen :: (Array AST -> AST') -> Source' -> String -> TokenInfo
parseParen make start right =
  { priority: top
  , parse: \_ priority output -> do
      a <- parseParen' make start right empty
      parse' priority (snoc output a) }


errorMissing :: forall a. Source' -> String -> TokenInfo
errorMissing source left =
  { priority: bottom
  , parse: \_ _ _ ->
      throwError $ MissingStartParen left source }


parseTransform :: Priority -> Associativity -> (Queue AST -> Token -> Queue AST -> Parser (Queue AST)) -> TokenInfo
parseTransform priority associativity parse =
  { priority: priority
  , parse: \token continue output -> do
      let priority' = case associativity of
                        Left -> priority
                        Right -> priority - 1
      right <- parse' priority' empty
      a <- parse output token right
      parse' continue a }


parsePrefix :: Priority -> (AST -> AST') -> Source' -> String -> TokenInfo
parsePrefix priority make middle name =
  parseTransform priority Right \left _ right ->
    case uncons right of
      Just (Tuple r right) ->
        pure $ snoc left (Source (make r) (middle <> source r)) <> right

      Nothing ->
        throwError $ MissingRightExpression name middle


parseInfix :: Priority -> Associativity -> (AST -> AST -> AST') -> Source' -> String -> TokenInfo
parseInfix priority associativity make middle name =
  parseTransform priority associativity \left _ right ->
    case unsnoc left of
      Just (Tuple l left) ->
        case uncons right of
          Just (Tuple r right) ->
            pure $ snoc left (Source (make l r) (source l <> source r)) <> right

          Nothing ->
            throwError $ MissingRightExpression name middle

      Nothing ->
        throwError $ MissingLeftExpression name middle


parseWildcard :: Source' -> TokenInfo
parseWildcard middle =
  { priority: top
  , parse: \token priority output ->
      parse' priority $ snoc output $ Source Wildcard middle }


parseLambda :: Priority -> (Array AST -> AST -> AST') -> Source' -> String -> TokenInfo
parseLambda priority make middle name =
  parseTransform priority Right \left _ right ->
    case unsnoc right of
      Just (Tuple r right) ->
        pure $ snoc left $ Source (make (Array.fromFoldable right) r) (middle <> source r)

      Nothing ->
        throwError $ LambdaMissingBody name middle


parseNormal :: TokenInfo
parseNormal =
  { priority: top
  , parse: \token priority output ->
      parse' priority $ snoc output $ map tokenToAST token }


parseSpecial :: Token -> TokenInfo
parseSpecial (Source (TokenSymbol "(") source) = parseParen Parens source ")"
parseSpecial (Source (TokenSymbol ")") source) = errorMissing source "("
parseSpecial (Source (TokenSymbol "[") source) = parseParen Array source "]"
parseSpecial (Source (TokenSymbol "]") source) = errorMissing source "["
parseSpecial (Source (TokenSymbol "{") source) = parseParen Record source "}"
parseSpecial (Source (TokenSymbol "}") source) = errorMissing source "{"
parseSpecial (Source (TokenSymbol "&") source) = parsePrefix 10 Quote source "&"
parseSpecial (Source (TokenSymbol "~") source) = parsePrefix 20 Unquote source "~"
parseSpecial (Source (TokenSymbol "@") source) = parsePrefix 20 Splice source "@"
parseSpecial (Source (TokenSymbol ".") source) = parseInfix 10 Left Dot source "."
parseSpecial (Source (TokenSymbol ":") source) = parseInfix 10 Right Match source ":"
parseSpecial (Source (TokenSymbol "::") source) = parseInfix 10 Right Type source "::"
parseSpecial (Source (TokenSymbol "<=") source) = parseInfix 10 Right Assign source "<="
parseSpecial (Source (TokenSymbol "->") source) = parseLambda 10 Lambda source "->"
parseSpecial (Source (TokenSymbol "_") source) = parseWildcard source
parseSpecial _ = parseNormal


parse'' :: Priority -> Queue AST -> Token -> Queue Token -> Parser (Queue AST)
parse'' priority output token rest =
  let a = parseSpecial token
  in
    if priority < a.priority
    then do
      setCurrent rest
      a.parse token priority output
    else
      pure output


parse' :: Priority -> Queue AST -> Parser (Queue AST)
parse' priority output = do
  current <- getCurrent
  case current of
    Just (Tuple token rest) ->
      parse'' priority output token rest

    Nothing ->
      pure output


parse :: Either ParseError (Queue Token) -> Either ParseError (Queue AST)
parse input = do
  input <- input
  runState
    (parse' bottom empty)
    { input: input }
