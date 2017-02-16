module Nulan.Tokenize where

import Prelude
import Data.String (fromCharArray, toCharArray, singleton, charAt)
import Data.Maybe (Maybe(..), isNothing)
import Data.Either (Either(..))

import Nulan.Queue (Queue, uncons, snoc, empty, fromString)
import Nulan.ParseError (ParseError(..))
import Nulan.AST (AST(..))
import Nulan.Position (Position(..), incrementPosition)
import Nulan.Source (Source'(..), Source(..))
import Nulan.State (State, runState, getState, modifyState, throwError)

import Debug.Trace


data Token'
  = TokenInteger String
  | TokenText String
  | TokenSymbol String

instance showToken :: Show Token' where
  show (TokenInteger s) = s
  show (TokenText s) = show s
  show (TokenSymbol s) = s

type Token = Source Token'


type TokenizerState =
  { input :: String
  , filename :: String
  , position :: Position }

type Tokenizer = State ParseError TokenizerState


getPosition :: Tokenizer Position
getPosition = do
  { position } <- getState
  pure position


getSource :: Position -> Tokenizer Source'
getSource start = do
  { filename, position } <- getState
  pure $ Source' { filename: filename
                 , start: start
                 , end: position }


peekChar :: Tokenizer (Maybe Char)
peekChar = do
  { input, position: Position { index } } <- getState
  pure $ charAt index input


readChar :: Char -> Tokenizer Unit
readChar char = modifyState \{ input, filename, position } ->
  { input
  , filename
  , position: incrementPosition char position }


error :: forall a. (Source' -> ParseError) -> Position -> Tokenizer a
error f start = do
  source <- getSource start
  throwError (f source)


consume1 :: Char -> Queue Token -> Tokenizer (Queue Token)
consume1 char output = do
  readChar char
  pure output


push1 :: Char -> Queue Token -> Tokenizer (Queue Token)
push1 char output = do
  start <- getPosition
  readChar char
  source <- getSource start
  pure $ snoc output $ Source (TokenSymbol $ singleton char) source


error1 :: forall a. Char -> (Source' -> ParseError) -> Queue Token -> Tokenizer a
error1 char f output = do
  start <- getPosition
  readChar char
  error f start


tokenizeBlockComment' :: (Maybe Char) -> Position -> Tokenizer Unit
tokenizeBlockComment' char start = case char of
  Just char -> do
    readChar char
    tokenizeBlockComment start

  Nothing -> do
    source <- getSource start
    throwError $ MissingEndComment source


tokenizeBlockComment :: Position -> Tokenizer Unit
tokenizeBlockComment start = do
  char <- peekChar
  case char of
    Just '#' -> do
      subStart <- getPosition
      readChar '#'
      char <- peekChar
      case char of
        Just '/' -> do
          readChar '/'
          tokenizeBlockComment subStart
          tokenizeBlockComment start

        char ->
          tokenizeBlockComment' char start

    Just '/' -> do
      readChar '/'
      char <- peekChar
      case char of
        Just '#' -> do
          readChar '#'
          pure unit

        char ->
          tokenizeBlockComment' char start

    char ->
      tokenizeBlockComment' char start


tokenizeComment :: Queue Token -> Tokenizer (Queue Token)
tokenizeComment output = do
  start <- getPosition
  readChar '#'
  char <- peekChar
  case char of
    Just '/' -> do
      readChar '/'
      tokenizeBlockComment start
      pure output

    Just char -> do
      readChar char
      -- TODO Windows/Mac line endings ?
      _ <- consumeMany (_ /= '\n') (singleton char)
      pure output

    Nothing ->
      pure output


tokenizeSpecial :: Char -> Maybe (Queue Token -> Tokenizer (Queue Token))
tokenizeSpecial '#'  = Just tokenizeComment
--tokenizeSpecial '"'  = Just (consume1 '"')
tokenizeSpecial '\t' = Just (error1 '\t' TabsNotAllowed)
tokenizeSpecial '\n' = Just (consume1 '\n')
tokenizeSpecial ' '  = Just (consume1 ' ')
tokenizeSpecial '('  = Just (push1 '(')
tokenizeSpecial ')'  = Just (push1 ')')
tokenizeSpecial '['  = Just (push1 '[')
tokenizeSpecial ']'  = Just (push1 ']')
tokenizeSpecial '{'  = Just (push1 '{')
tokenizeSpecial '}'  = Just (push1 '}')
tokenizeSpecial '@'  = Just (push1 '@')
tokenizeSpecial '.'  = Just (push1 '.')
tokenizeSpecial '&'  = Just (push1 '&')
tokenizeSpecial '~'  = Just (push1 '~')
tokenizeSpecial _    = Nothing


consumeMany :: (Char -> Boolean) -> String -> Tokenizer String
consumeMany test str = do
  char <- peekChar
  case char of
    Just char ->
      if test char
      then do
        readChar char
        consumeMany test (str <> singleton char)
      else
        pure str
    Nothing ->
      pure str


isIdentifier :: Char -> Boolean
isIdentifier char = isNothing (tokenizeSpecial char)


tokenizeIdentifier :: Char -> Queue Token -> Tokenizer (Queue Token)
tokenizeIdentifier char output = do
  start <- getPosition
  readChar char
  str <- consumeMany isIdentifier (singleton char)
  source <- getSource start
  pure $ snoc output $ Source (TokenSymbol str) source


tokenize' :: Queue Token -> Tokenizer (Queue Token)
tokenize' output = do
  char <- peekChar
  case char of
    Just char -> do
      output <- case tokenizeSpecial char of
        Nothing -> tokenizeIdentifier char output
        Just f -> f output
      tokenize' output
    Nothing ->
      pure output


tokenize :: String -> String -> Either ParseError (Queue Token)
tokenize code filename =
  runState
    (tokenize' empty)
    { input: code
    , filename: filename
    , position: Position { index: 0, line: 0, column: 0 } }
