module Nulan.Parse where

import Prelude
import Nulan.CharStream (CharStream, fromString, peek, next)
import Data.String (fromCharArray, toCharArray)
import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))
import Data.Either (Either)
import Data.Array (many, some)
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Text.Parsing.Parser.Expr (buildExprParser, Assoc(..), Operator(..))
import Text.Parsing.Parser.Combinators (optionMaybe, skipMany, between)
import Text.Parsing.Parser.String (char, eof, oneOf, string)
import Nulan.AST (AST(..))
import Debug.Trace


data Token
  = TokenInteger String Source
  | TokenNumber String Source
  | TokenText String Source
  | TokenSymbol String Source



tokenize1 :: Char -> Maybe Token


digit :: Parser String Char
digit = oneOf $ toCharArray "0123456789"

identifier :: Parser String Char
identifier = oneOf $ toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ?!*$<=-"


parseParens :: Unit -> Parser String AST
parseParens _ = do
  a <- between (char '(') (char ')') parse'
  pure $ Parens a


parseArray :: Unit -> Parser String AST
parseArray _ = do
  a <- between (char '[') (char ']') parse'
  pure $ Array a


parseRecord :: Unit -> Parser String AST
parseRecord _ = do
  a <- between (char '{') (char '}') parse'
  pure $ Record a


parseWhitespace :: Parser String Char
parseWhitespace = char ' ' <|> char '\n'


parseNums :: Parser String AST
parseNums = do
  a <- some digit
  b <- optionMaybe (char '.')
  case b of
    Just b -> do
      c <- some digit
      pure $ Number $ fromCharArray (a <> [b] <> c)
    Nothing ->
      pure $ Integer $ fromCharArray a


parseIdentifier :: Parser String AST
parseIdentifier = do
  a <- some identifier
  pure $ Symbol $ fromCharArray a


parse' :: Parser String (Array AST)
parse' = many do
  skipMany parseWhitespace
  buildExprParser
    [ [ Infix (string "<=" $> \a b -> Assign (spy a) (spy b)) AssocRight ]
    , [ Infix (string "::" $> Type) AssocLeft ]
    ]
    (parseParens unit <|>
     parseArray unit <|>
     parseRecord unit <|>
     parseNums <|>
     parseIdentifier)


parse :: String -> String -> Either ParseError (Array AST)
parse code filename = runParser code do
  a <- parse'
  -- TODO is this the idiomatic way to accomplish this ?
  eof
  pure a
