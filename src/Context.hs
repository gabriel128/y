module Context where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

-- Locals Vars are meant to be unique.
-- But there is no place in the stack ensured
type Locals = Set T.Text

type StackOffset = Int

data Context = Context {ctxLocals :: !Locals, ctxStackOffset :: !StackOffset}
  deriving (Show, Eq)

defaultContext :: Context
defaultContext = Context Set.empty 0

addLocal :: T.Text -> Context -> Context
addLocal localVar (Context locals sOffet) = Context (Set.insert localVar locals) sOffet

localsList :: Context -> [T.Text]
localsList = Set.toAscList . ctxLocals
