module Passes.PassEffs where

import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Context (Context)

data PassError
  = PassError Text
  | GenericError Text
  deriving (Show)

type StErr sig m a = (Has (State Context) sig m, Has (Throw Text) sig m) => m a

type StErrRnd sig m a = (Has (State Context) sig m, Has (Throw Text) sig m, Has Fresh sig m) => m a

runStErr :: Context -> StateC Context (ErrorC Text Identity) a -> Either Text (Context, a)
runStErr info = run . runError . runState info
