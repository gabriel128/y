module Irs.X86 where

import qualified Data.Text as T

-- reg ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
-- arg ::=  $int | %reg | int(%reg)
-- instr ::= addq  arg, arg | subq  arg, arg | negq  arg | movq  arg, arg | callq label | pushq arg | popq arg | retq
-- prog ::=  .globl main
--         main:  instr^{+}

type Label = T.Text

data Arg = Imm Int | Reg | RegOffset Reg Int deriving (Eq, Show)

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

data Reg
  = Rsp
  | Rbp
  | Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Eq, Show)

newtype X86Program = X86Program [Instr]
