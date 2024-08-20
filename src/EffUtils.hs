module EffUtils where

import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Data.Functor.Identity (Identity)

-- | An effect that handles state and error
type StateErrorEff state error a =
  forall sig m. (Has (State state) sig m, Has (Throw error) sig m) => m a

-- | An effect that handles state and error
type StateErrorRndEff state error a =
  forall sig m. (Has (State state) sig m, Has (Throw error) sig m, Has Fresh sig m) => m a

-- | Same as above but with a flexible m
type StateErrorEffM state error m a =
  forall sig. (Has (State state) sig m, Has (Throw error) sig m) => m a

type StateErrorRndEffM state error m a =
  forall sig. (Has (State state) sig m, Has (Throw error) sig m, Has Fresh sig m) => m a


-- | Utility function to extract the state and the error out of the effect
runStateErrorEff :: state -> StateC state (ErrorC errorType Identity) a -> Either errorType (state, a)
runStateErrorEff someState = run . runError  . runState someState
