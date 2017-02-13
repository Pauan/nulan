module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Nulan.Parse
import Debug.Trace

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  traceShowA (parse " [10.0]  ((20.0) 30) foobar <= test" "tmp.nul")
