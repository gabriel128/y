module Lib where

import Ast.Ast
import Data.Text (Text)
import qualified Irs.Atomize as Atomize
import qualified Irs.X86Var as X86Var

compile :: Program -> Either Text Text
compile (Program info stmts) =
  do
    let atomized = Atomize.removeComplexStmts makeDefaultInfo stmts
    x86Var <- X86Var.fromAst atomized
    pure ""
