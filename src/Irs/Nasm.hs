{-# LANGUAGE QuasiQuotes #-}

-- | Represents type checked X86 in NASM format
module Irs.Nasm where

-- reference https://www.cs.virginia.edu/~evans/cs216/guides/x86.html
-- Grammar
-- reg ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
-- arg ::=  $int | %reg | int(%reg)
-- instr ::= addq  arg, arg | subq  arg, arg | negq  arg | movq  arg, arg | callq label | pushq arg | popq arg | retq
-- prog ::=  .globl main
--         main:  instr^{+}

import Data.List
import Data.Text (toLower)
import qualified Data.Text as T
import Data.Word
import NeatInterpolation

class Print s where
  textPrint :: s -> T.Text

type Label = T.Text

type Offset = Word8

newtype Imm = Imm Word32 deriving (Eq)

instance Print Imm where
  textPrint (Imm num) = T.pack $ show num

-- A memory address (e.g., [eax], [var + 4], or dword ptr [eax+ebx])
data MemDeref = MemDeref Reg Offset deriving (Eq)

instance Print MemDeref where
  textPrint (MemDeref reg offset) = T.pack $ "[" <> T.unpack (textPrint reg) <> "+" <> show offset <> "]"

-- lea edi, [ebx+4*esi] — the quantity EBX+4*ESI is placed in EDI.
-- lea eax, [var] — the value in var is placed in EAX.
-- lea eax, [val] — the value val is placed in EAX.
data Lea = Lea -- TODO

data UnaryMode = UnaryReg Reg | UnaryMem MemDeref

instance Print UnaryMode where
  textPrint (UnaryReg reg) = textPrint reg
  textPrint (UnaryMem mem) = textPrint mem

data PushMode = PImm Imm | PMem MemDeref | PReg Reg

instance Print PushMode where
  textPrint (PImm imm) = textPrint imm
  textPrint (PMem mem) = textPrint mem
  textPrint (PReg reg) = textPrint reg

-- e.g.
-- mov <reg>,<reg>
-- mov <reg>,<mem>
-- mov <mem>,<reg>
-- mov <reg>,<const>
-- mov <mem>,<const>
data BinaryMode
  = Regs Reg Reg
  | MemReg Reg MemDeref
  | RegMem MemDeref Reg
  | ImmReg Reg Imm
  | ImmMem MemDeref Imm
  deriving (Eq)

instance Print BinaryMode where
  textPrint (Regs reg1 reg2) = textPrint reg1 <> "," <> textPrint reg2
  textPrint (MemReg reg mem) = textPrint reg <> "," <> textPrint mem
  textPrint (RegMem mem reg) = textPrint mem <> "," <> textPrint reg
  textPrint (ImmReg reg imm) = textPrint reg <> "," <> textPrint imm
  textPrint (ImmMem mem imm) = textPrint mem <> "," <> textPrint imm

data Reg = Rsp | Rbp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Show)

instance Print Reg where
  textPrint reg = toLower . T.pack $ show reg

data Instr
  = Add BinaryMode
  | Sub BinaryMode
  | Neg UnaryMode
  | Mov BinaryMode
  | Call Label
  | Push PushMode
  | Pop PushMode
  | Ret
  | Jmp Label

instance Print Instr where
  textPrint (Add binaryMode) = "add " <> textPrint binaryMode
  textPrint (Sub binaryMode) = "sub " <> textPrint binaryMode
  textPrint (Neg unaryMode) = "neg " <> textPrint unaryMode
  textPrint (Mov binaryMode) = "mov " <> textPrint binaryMode
  textPrint (Call label) = "call " <> label
  textPrint (Push mode) = "push " <> textPrint mode
  textPrint (Pop mode) = "pop " <> textPrint mode
  textPrint Ret = "ret"
  textPrint (Jmp label) = T.pack $ "jmp " <> show label

-- .global main
-- main:
--     pushq   rbp
--     movq    rbp, rsp
--     subq    rsp, 16
--     jmp start

-- start:
--     < put program here>
--     jmp   conclusion

-- conclusion:
--     addq    rsp, rbp
--     popq    rbp
--     ret
programTemplate :: T.Text -> Word8 -> T.Text
programTemplate logic stackOffset =
  let offset = T.pack $ show stackOffset
   in [trimming|
        .global main
      main:
          pushq   rbp
          movq    rbp, rsp
          subq    rsp, $offset
          jmp start

      start:
        $logic
        jmp conclusion

      conclusion:
          addq    rsp, rbp
          popq    rbp
          ret
    |]

printInstrs :: [Instr] -> T.Text
printInstrs instrs = T.concat $ intersperse " \n" (fmap textPrint instrs)
