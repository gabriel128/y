module EffUtils where

import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Data.Functor.Identity (Identity)
import Data.Text (Text)

type StateErrorEff state error a =
  forall sig m. (Has (State state) sig m, Has (Throw error) sig m) => m a

type StateErrorRndEff state error a =
  forall sig m. (Has (State state) sig m, Has (Throw error) sig m, Has Fresh sig m) => m a

-- M version to make M flexible
type StateErrorEffM state error m a =
  forall sig. (Has (State state) sig m, Has (Throw error) sig m) => m a

type StateErrorRndEffM state error m a =
  forall sig. (Has (State state) sig m, Has (Throw error) sig m, Has Fresh sig m) => m a

runStateErrorEff :: state -> StateC state (ErrorC Text Identity) a -> Either Text (state, a)
runStateErrorEff someState = run . runError . runState someState
