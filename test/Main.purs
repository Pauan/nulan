module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Control.Monad.Aff.AVar (AVAR)

import Test.Nulan.Tokenize as Tokenize

main :: Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR) Unit
main = runTest do
  Tokenize.tests
