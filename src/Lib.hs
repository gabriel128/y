module Lib where

import Ast.TypedAst (TypedProgram)
import Context (Context, defaultContext)
import Data.Text (Text)
import EffUtils (StateErrorEff, runStateErrorEff)
import Parser.Parser (runProgramParser)
import qualified Passes.Atomizer as Atomizer
import qualified Passes.StmtsToX86 as StmtsToX86
import qualified Passes.TypeChecker as TypeChecker
import qualified Passes.X86ToTextProg

parseAndCompile :: Text -> Either Text (Context, Text)
parseAndCompile text = do
  prog <- runProgramParser text
  runStateErrorEff Context.defaultContext (passes prog)

passes :: TypedProgram -> StateErrorEff Context Text Text
passes prog = do
  prog' <- TypeChecker.typeCheck prog
  prog'' <- Atomizer.removeComplexStmts prog'
  nasmInstrs <- StmtsToX86.astToNasm prog''
  Passes.X86ToTextProg.instrsToText nasmInstrs
