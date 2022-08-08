module Irs.X86Var where

import qualified Data.Text as T

type Label = T.Text

data Arg = Imm Int | Var T.Text deriving (Eq, Show)

data Instr
  = Add Arg Arg
  | Sub Arg Arg
  | Neg Arg
  | Mov Arg Arg
  | Call Label
  | Push Arg
  | Pop Arg
  | Ret
  | Jmp Label
  deriving (Eq, Show)

newtype X86Var = X86Var [Instr]
