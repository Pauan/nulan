module Test.Nulan.Tokenize where

import Prelude
import Data.Either (Either(..))
import Data.Array as Array
import Test.Unit (suite, TestSuite, failure, test)
import Test.Unit.Assert (equal)
import Nulan.Tokenize (Token, Token'(..), tokenize)
import Nulan.Position (Position, position)
import Nulan.Source (Source, source)

source' :: forall a. a -> Position -> Position -> Source a
source' a start end = source a "test.nul" start end

testTokenize :: forall a. String -> Source (Array Token) -> TestSuite a
testTokenize input a =
  test input
    case tokenize input "test.nul" of
      Left b -> failure (show b)
      Right b -> equal a (map Array.fromFoldable b)

tests :: forall a. TestSuite a
tests = suite "Tokenize" do
  testTokenize "( foo bar )" $ source'
    [ source' (TokenSymbol "(") (position 0 0 0) (position 1 0 1)
    , source' (TokenSymbol "foo") (position 2 0 2) (position 5 0 5)
    , source' (TokenSymbol "bar") (position 6 0 6) (position 9 0 9)
    , source' (TokenSymbol ")") (position 10 0 10) (position 11 0 11) ]
    (position 0 0 0)
    (position 11 0 11)

  testTokenize " [10.0]  &((20.0) 30) (-> 1 2 3 4) { _ }\nfoobar <= test" $ source'
    [ source' (TokenSymbol "[") (position 1 0 1) (position 2 0 2)
    , source' (TokenInteger "10") (position 2 0 2) (position 4 0 4)
    , source' (TokenSymbol ".") (position 4 0 4) (position 5 0 5)
    , source' (TokenInteger "0") (position 5 0 5) (position 6 0 6)
    , source' (TokenSymbol "]") (position 6 0 6) (position 7 0 7)
    , source' (TokenSymbol "&") (position 9 0 9) (position 10 0 10)
    , source' (TokenSymbol "(") (position 10 0 10) (position 11 0 11)
    , source' (TokenSymbol "(") (position 11 0 11) (position 12 0 12)
    , source' (TokenInteger "20") (position 12 0 12) (position 14 0 14)
    , source' (TokenSymbol ".") (position 14 0 14) (position 15 0 15)
    , source' (TokenInteger "0") (position 15 0 15) (position 16 0 16)
    , source' (TokenSymbol ")") (position 16 0 16) (position 17 0 17)
    , source' (TokenInteger "30") (position 18 0 18) (position 20 0 20)
    , source' (TokenSymbol ")") (position 20 0 20) (position 21 0 21)
    , source' (TokenSymbol "(") (position 22 0 22) (position 23 0 23)
    , source' (TokenSymbol "->") (position 23 0 23) (position 25 0 25)
    , source' (TokenInteger "1") (position 26 0 26) (position 27 0 27)
    , source' (TokenInteger "2") (position 28 0 28) (position 29 0 29)
    , source' (TokenInteger "3") (position 30 0 30) (position 31 0 31)
    , source' (TokenInteger "4") (position 32 0 32) (position 33 0 33)
    , source' (TokenSymbol ")") (position 33 0 33) (position 34 0 34)
    , source' (TokenSymbol "{") (position 35 0 35) (position 36 0 36)
    , source' (TokenSymbol "_") (position 37 0 37) (position 38 0 38)
    , source' (TokenSymbol "}") (position 39 0 39) (position 40 0 40)
    , source' (TokenSymbol "foobar") (position 41 1 0) (position 47 1 6)
    , source' (TokenSymbol "<=") (position 48 1 7) (position 50 1 9)
    , source' (TokenSymbol "test") (position 51 1 10) (position 55 1 14) ]
    (position 0 0 0)
    (position 55 1 14)
