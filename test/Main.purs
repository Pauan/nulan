module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Control.Monad.Aff.AVar (AVAR)

import Test.Nulan.Tokenize as Tokenize
import Test.Nulan.Parse as Parse
import Test.Nulan.Compile as Compile
import Test.Nulan.Compile.JavaScript as JavaScript

main :: Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR) Unit
main = runTest do
  Tokenize.tests
  Parse.tests
  Compile.tests
  JavaScript.tests
