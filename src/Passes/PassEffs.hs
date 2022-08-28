module Passes.PassEffs where

import qualified Ast.Ast as Ast
import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.State.Strict
import Data.Functor.Identity (Identity)
import Data.Text (Text)

data PassError
  = PassError Text
  | GenericError Text
  deriving (Show)

type StErr sig m a = (Has (State Ast.Info) sig m, Has (Throw Text) sig m) => m a

type StErrRnd sig m a = (Has (State Ast.Info) sig m, Has (Throw Text) sig m, Has Fresh sig m) => m a

runStErr :: Ast.Info -> StateC Ast.Info (ErrorC Text Identity) a -> Either Text (Ast.Info, a)
runStErr info = run . runError . runState info
