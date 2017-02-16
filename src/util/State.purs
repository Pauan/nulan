module Nulan.State where

import Prelude
import Control.Monad.Except as ExceptT
import Control.Monad.State as StateT
import Data.Either (Either)


type State e s = ExceptT.ExceptT e (StateT.State s)


getState :: forall e s. State e s s
getState = StateT.get


putState :: forall e s. s -> State e s Unit
putState = StateT.put


modifyState :: forall e s. (s -> s) -> State e s Unit
modifyState = StateT.modify


throwError :: forall e s a. e -> State e s a
throwError = ExceptT.throwError


runState :: forall e s a. State e s a -> s -> Either e a
runState state init =
  StateT.evalState (ExceptT.runExceptT state) init
