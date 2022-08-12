module Lib where

import Ast.Ast
import qualified Irs.Atomize as Atomize

compile :: Program -> Program
compile (Program stmts) = Program (concatMap Atomize.removeComplexStmt stmts)
