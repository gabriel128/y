module Lib where

import Ast.Ast (Info, Program, defaultInfo)
import Data.Text (Text)
import Irs.Atomize (removeComplexStmts)
import Irs.PassEffs (runStErr)
import qualified Irs.PassEffs as PassEffs
import Irs.StmtsToX86 (astToNasm)
import Irs.X86ToTextProg (instrsToText)

compile :: Program -> Either Text (Info, Text)
compile prog = runStErr defaultInfo (passes prog)

passes :: Program -> PassEffs.StErr sig m Text
passes prog = do
  prog' <- removeComplexStmts prog
  nasmInstrs <- astToNasm prog'
  instrsToText nasmInstrs
