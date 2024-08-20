module Lib where

import Ast.Ast (Program)
import Data.Text (Text)
import Parser.Parser (runProgramParser)
import qualified Passes.Atomizer as Atomizer
import EffUtils (runStateErrorEff, StateErrorEff)
import Context (Context, defaultContext)
import qualified Passes.StmtsToX86 as StmtsToX86
import qualified Passes.X86ToTextProg

parseAndCompile :: Text -> Either Text (Context, Text)
parseAndCompile text = do
  prog <- runProgramParser text
  runStateErrorEff Context.defaultContext (passes prog)

passes :: Program -> StateErrorEff Context Text Text
passes prog = do
  prog' <- Atomizer.removeComplexStmts prog
  nasmInstrs <- StmtsToX86.astToNasm prog'
  Passes.X86ToTextProg.instrsToText nasmInstrs
