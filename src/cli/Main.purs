module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Nulan.Tokenize
import Nulan.Parse
import Debug.Trace

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (show (parse (tokenize " [10.0]  &((20.0) 30) (-> 1 2 3 4) { _ }\nfoobar <= test" "tmp.nul")))
