module Nasm.Data where

import Data.Text (Text, pack, toLower, unpack)
import Utils

type Label = Text

type Offset = Int

newtype Imm = Imm Int deriving (Eq)

data NasmError = InvalidAsm Text | GeneralError Text deriving (Show)

-- A memory address (e.g., [eax], [var + 4], or dword ptr [eax+ebx])
data MemDeref = MemDeref Reg Offset deriving (Eq, Show)

-- lea edi, [ebx+4*esi] — the quantity EBX+4*ESI is placed in EDI.
-- lea eax, [var] — the value in var is placed in EAX.
-- lea eax, [val] — the value val is placed in EAX.
data Lea = Lea -- TODO

data RMArg = RMr Reg | RMm MemDeref

data BinaryArgs
  = RR Reg Reg
  | MR MemDeref Reg
  | RM Reg MemDeref
  | RI Reg Imm
  | MI MemDeref Imm
  | RL Reg Label
  deriving (Eq)

data Reg = Rsp | Rbp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Show)

data Arg = ArgI Imm | ArgM MemDeref | ArgR Reg

data Instr
  = Add BinaryArgs
  | Sub BinaryArgs
  | Neg RMArg
  | Mov BinaryArgs
  | Xor BinaryArgs
  | Call Label
  | Push Arg
  | Pop Arg
  | Ret
  | Jmp Label

instance Print Reg where
  textPrint r = toLower . pack $ show r

instance Print Instr where
  textPrint (Add binaryMode) = "add " <> textPrint binaryMode
  textPrint (Sub binaryMode) = "sub " <> textPrint binaryMode
  textPrint (Neg unaryMode) = "neg " <> textPrint unaryMode
  textPrint (Mov binaryMode) = "mov " <> textPrint binaryMode
  textPrint (Call label) = "call " <> label
  textPrint (Push mode) = "push " <> textPrint mode
  textPrint (Pop mode) = "pop " <> textPrint mode
  textPrint (Xor mode) = "xor " <> textPrint mode
  textPrint Ret = "ret"
  textPrint (Jmp label) = pack $ "jmp " <> show label

instance Print MemDeref where
  textPrint (MemDeref r offset)
    | offset < 0 = pack $ "[" <> unpack (textPrint r) <> show offset <> "]"
    | offset > 0 = pack $ "[" <> unpack (textPrint r) <> "+" <> show offset <> "]"
    | offset == 0 = pack $ "[" <> unpack (textPrint r) <> "]"

instance Print RMArg where
  textPrint (RMr reg) = textPrint reg
  textPrint (RMm mem) = textPrint mem

instance Print Arg where
  textPrint (ArgI imm) = textPrint imm
  textPrint (ArgM mem) = textPrint mem
  textPrint (ArgR r) = textPrint r

instance Print Imm where
  textPrint (Imm num) = pack $ show num

instance Print BinaryArgs where
  textPrint (RR reg1 reg2) = textPrint reg1 <> "," <> textPrint reg2
  textPrint (MR r mem) = "qword " <> textPrint r <> "," <> textPrint mem
  textPrint (RM mem r) = textPrint mem <> "," <> textPrint r
  textPrint (RI r imm) = textPrint r <> "," <> textPrint imm
  textPrint (RL r text) = textPrint r <> "," <> text
  textPrint (MI mem imm) = "qword " <> textPrint mem <> "," <> textPrint imm
