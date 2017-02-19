module Test.Nulan.Parse where

import Prelude
import Data.Either (Either(..))
import Data.Array as Array
import Test.Unit (suite, TestSuite, failure, test)
import Test.Unit.Assert (equal)
import Nulan.AST (AST, AST'(..))
import Nulan.Parse (parse)
import Nulan.Tokenize (tokenize)
import Nulan.Position (Position, position)
import Nulan.Source (Source, source)
import Nulan.Pretty (pretty)

source' :: forall a. a -> Position -> Position -> Source a
source' a start end = source a "test.nul" start end

testParse :: forall a. String -> Array AST -> TestSuite a
testParse input a =
  test input
    case parse (tokenize input "test.nul") of
      Left b -> failure (show b)
      Right b -> equal a (Array.fromFoldable b)

testPretty :: forall a. String -> Array String -> TestSuite a
testPretty input a =
  test input
    case parse (tokenize input "test.nul") of
      Left b -> failure (show b)
      Right b -> equal a (map pretty (Array.fromFoldable b))

tests :: forall a. TestSuite a
tests = suite "Parse" do
  testParse "( foo bar )"
    [ source' (Parens [ source' (Symbol "foo") (position 2 0 2) (position 5 0 5)
                      , source' (Symbol "bar") (position 6 0 6) (position 9 0 9) ])
              (position 0 0 0)
              (position 11 0 11) ]


  testParse " [10.0]  &((20.0) 30) (-> 1 2 3 4) { _ }\nfoobar <= test"
    [ source' (Array [ source' (Number "10.0") (position 2 0 2) (position 6 0 6) ])
              (position 1 0 1)
              (position 7 0 7)

    , source' (Quote (source' (Parens [ source' (Parens [ source' (Number "20.0") (position 12 0 12) (position 16 0 16) ])
                                                (position 11 0 11)
                                                (position 17 0 17)
                                      , source' (Integer "30") (position 18 0 18) (position 20 0 20) ])
                              (position 10 0 10)
                              (position 21 0 21)))
              (position 9 0 9)
              (position 21 0 21)

    , source' (Parens [ source' (Lambda [ source' (Integer "1") (position 26 0 26) (position 27 0 27)
                                        , source' (Integer "2") (position 28 0 28) (position 29 0 29)
                                        , source' (Integer "3") (position 30 0 30) (position 31 0 31) ]
                                  (source' (Integer "4") (position 32 0 32) (position 33 0 33)))
                                (position 23 0 23)
                                (position 33 0 33) ])
              (position 22 0 22)
              (position 34 0 34)

    , source' (Record [ source' Wildcard (position 37 0 37) (position 38 0 38) ])
              (position 35 0 35)
              (position 40 0 40)

    , source' (Assign (source' (Symbol "foobar") (position 41 1 0) (position 47 1 6))
                      (source' (Symbol "test") (position 51 1 10) (position 55 1 14)))
              (position 41 1 0)
              (position 55 1 14) ]


  testPretty "(-> 1 2 3 (-> 4 5 6 [ 1 2 3 ] { 4 5 6 } | [ 1 &~2 ]))"
    [ "(->\n\
      \  1\n\
      \  2\n\
      \  3\n\
      \  (->\n\
      \    4\n\
      \    5\n\
      \    6\n\
      \    [ 1 2 3 ]\n\
      \    { 4 5 6 }\n\
      \    | [ 1 &~2 ]))" ]
