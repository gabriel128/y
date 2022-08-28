module Lib where

import Ast.Ast (Info, Program, defaultInfo)
import Data.Text (Text)
import Passes.AtomizeAst (removeComplexStmts)
import Passes.PassEffs (runStErr)
import qualified Passes.PassEffs as PassEffs
import Passes.StmtsToX86 (astToNasm)
import Passes.X86ToTextProg (instrsToText)

compile :: Program -> Either Text (Info, Text)
compile prog = runStErr defaultInfo (passes prog)

passes :: Program -> PassEffs.StErr sig m Text
passes prog = do
  prog' <- removeComplexStmts prog
  nasmInstrs <- astToNasm prog'
  instrsToText nasmInstrs
