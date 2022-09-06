module Lib where

import Ast.Ast (Info, Program, defaultInfo)
import Data.Text (Text)
import Parser.Parser (runProgramParser)
import Passes.AtomizeAst (removeComplexStmts)
import Passes.PassEffs (runStErr)
import qualified Passes.PassEffs as PassEffs
import Passes.StmtsToX86 (astToNasm)
import Passes.X86ToTextProg (instrsToText)

parseAndCompile :: Text -> Either Text (Info, Text)
parseAndCompile text = do
  prog <- runProgramParser text
  runStErr defaultInfo (passes prog)

passes :: Program -> PassEffs.StErr sig m Text
passes prog = do
  prog' <- removeComplexStmts prog
  nasmInstrs <- astToNasm prog'
  instrsToText nasmInstrs
