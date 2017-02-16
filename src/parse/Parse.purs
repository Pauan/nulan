module Nulan.Parse where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array as Array

import Nulan.Queue (Queue, uncons, snoc, empty, fromString)
import Nulan.ParseError (ParseError(..))
import Nulan.AST (AST, AST'(..))
import Nulan.Position (Position(..))
import Nulan.Source (Source(..), Source')
import Nulan.Tokenize (Token, Token'(..))
import Nulan.State (State, runState, getState, putState, throwError)

import Debug.Trace


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


tokenToAST :: Token -> AST
tokenToAST (Source (TokenInteger a) b) = Source (Integer a) b
tokenToAST (Source (TokenText a) b)    = Source (Text a) b
tokenToAST (Source (TokenSymbol a) b)  = Source (Symbol a) b


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


parseNormal :: TokenInfo
parseNormal =
  { priority: top
  , parse: \token priority output ->
      parse' priority $ snoc output $ tokenToAST token }


parseSpecial :: Token -> TokenInfo
parseSpecial (Source (TokenSymbol "(") source) = parseParen Parens source ")"
parseSpecial (Source (TokenSymbol ")") source) = errorMissing source "("
parseSpecial (Source (TokenSymbol "[") source) = parseParen Array source "]"
parseSpecial (Source (TokenSymbol "]") source) = errorMissing source "["
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
