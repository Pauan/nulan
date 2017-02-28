module Test.Nulan.Compile where

import Prelude
import Data.Either (Either(..))
import Data.Array as Array
import Test.Unit (suite, TestSuite, failure, test)
import Test.Unit.Assert (equal)
import Nulan.Parse (parse)
import Nulan.Tokenize (tokenize)
import Nulan.Compile (compile, Statement, Statement'(..), Expression, Expression'(..))
import Nulan.Position (Position, position)
import Nulan.Source (Source, source)
import Nulan.Pretty (pretty)

variableId :: Int -> Int
variableId a = a + 2

variable :: Int -> Expression'
variable a = Variable $ variableId a

source' :: forall a. a -> Position -> Position -> Source a
source' a start end = source a "test.nul" start end

testCompile :: forall a. String -> Statement -> TestSuite a
testCompile input a =
  test input
    case compile (parse (tokenize input "test.nul")) of
      Left b -> failure (show b)
      Right b -> equal a b

tests :: forall a. TestSuite a
tests = suite "Compile" do
  testCompile "(CONSTANT foo :: Int32 1)" $ source'
    (Statements
      [ source' (Constant
                  (source' (variableId 0) (position 10 0 10) (position 13 0 13))
                  (source' (Int32 1) (position 23 0 23) (position 24 0 24)))
                (position 0 0 0)
                (position 25 0 25) ])
    (position 0 0 0)
    (position 25 0 25)

  testCompile "(RECURSIVE (CONSTANT foo :: (FORALL a (-> a)) (-> (foo))))" $ source'
    (Statements
      [ source' (Recursive
                  [ source' (Constant
                              (source' (variableId 0) (position 21 0 21) (position 24 0 24))
                              (source' (Function []
                                         (source' (FunctionReturn
                                                    (source' (FunctionCall (source' (variable 0)
                                                                                    (position 51 0 51)
                                                                                    (position 54 0 54)) [])
                                                             (position 50 0 50)
                                                             (position 55 0 55)))
                                                  (position 50 0 50)
                                                  (position 55 0 55)))
                                       (position 46 0 46)
                                       (position 56 0 56)))
                            (position 11 0 11)
                            (position 57 0 57) ])
                (position 0 0 0)
                (position 58 0 58) ])
    (position 0 0 0)
    (position 58 0 58)


  testCompile "(RECURSIVE (CONSTANT foo :: (FORALL a (-> a)) (-> (bar))) (CONSTANT bar :: (FORALL a (-> a)) (-> (foo))))" $ source'
    (Statements
      [ source' (Recursive
                  [ source' (Constant
                              (source' (variableId 0) (position 21 0 21) (position 24 0 24))
                              (source' (Function []
                                         (source' (FunctionReturn
                                                    (source' (FunctionCall (source' (variable 1)
                                                                                    (position 51 0 51)
                                                                                    (position 54 0 54)) [])
                                                             (position 50 0 50)
                                                             (position 55 0 55)))
                                                  (position 50 0 50)
                                                  (position 55 0 55)))
                                       (position 46 0 46)
                                       (position 56 0 56)))
                            (position 11 0 11)
                            (position 57 0 57)
                  , source' (Constant
                              (source' (variableId 1) (position 68 0 68) (position 71 0 71))
                              (source' (Function []
                                         (source' (FunctionReturn
                                                    (source' (FunctionCall (source' (variable 0)
                                                                                    (position 98 0 98)
                                                                                    (position 101 0 101)) [])
                                                             (position 97 0 97)
                                                             (position 102 0 102)))
                                                  (position 97 0 97)
                                                  (position 102 0 102)))
                                       (position 93 0 93)
                                       (position 103 0 103)))
                            (position 58 0 58)
                            (position 104 0 104) ])
                (position 0 0 0)
                (position 105 0 105) ])
    (position 0 0 0)
    (position 105 0 105)

{-
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
  -}
