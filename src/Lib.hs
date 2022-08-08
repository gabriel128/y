module Lib where

import Ast.Ast
import qualified Irs.Id as Id

compile :: Program -> Program
compile (Program stmts) = Program (concatMap Id.removeComplexStmt stmts)
