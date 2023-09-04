{-# LANGUAGE GADTs #-}

module Nasm.Data where

import Data.Text (Text, pack, toLower, unpack)
import Utils

data NasmError = InvalidAsm Text | GeneralError Text deriving (Show)

-- New definitions
type Label = Text

type Offset = Int

type Imm = Int

-- A memory address (e.g., [eax], [var + 4], or dword ptr [eax+ebx])
data MemDeref = Deref Reg Offset deriving (Eq, Show)

data Reg = Rsp | Rbp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Show)

-- Allowed Binary arguments
class IsSimpleArg a

class AllowedBinAargs a b where
  isMemDest :: a -> b -> Bool

class IsMemOrReg a

-- TODO: add
-- lea edi, [ebx+4*esi] — the quantity EBX+4*ESI is placed in EDI.
-- lea eax, [var] — the value in var is placed in EAX.
-- lea eax, [val] — the value val is placed in EAX.
data Instr where
  Mov :: (Print a, Print b, Show b, AllowedBinAargs a b) => a -> b -> Instr
  Add :: (Print a, Print b, Show b, AllowedBinAargs a b) => a -> b -> Instr
  Sub :: (Print a, Print b, Show b, AllowedBinAargs a b) => a -> b -> Instr
  Xor :: (Print a, Print b, Show b, AllowedBinAargs a b) => a -> b -> Instr
  Neg :: (Print a, IsMemOrReg a) => a -> Instr
  Call :: Label -> Instr
  Push :: (Print a, IsSimpleArg a) => a -> Instr
  Pop :: (Print a, IsSimpleArg a) => a -> Instr
  Ret :: Instr
  Jmp :: Label -> Instr

-- Lea ::

instance IsSimpleArg Imm

instance IsSimpleArg MemDeref

instance IsSimpleArg Reg

instance AllowedBinAargs Reg Reg where
  isMemDest _ _ = False

instance AllowedBinAargs MemDeref Reg where
  isMemDest _ _ = True

instance AllowedBinAargs Reg MemDeref where
  isMemDest _ _ = False

instance AllowedBinAargs Reg Int where
  isMemDest _ _ = False

instance AllowedBinAargs Reg Text where
  isMemDest _ _ = False

instance AllowedBinAargs MemDeref Int where
  isMemDest _ _ = True

instance IsMemOrReg Reg

instance IsMemOrReg MemDeref

instance Print Reg where
  textPrint r = toLower . pack $ show r

instance Print MemDeref where
  textPrint (Deref r offset) =
    case compare offset 0 of
      LT -> pack $ "[" <> unpack (textPrint r) <> show offset <> "]"
      GT -> pack $ "[" <> unpack (textPrint r) <> "+" <> show offset <> "]"
      EQ -> pack $ "[" <> unpack (textPrint r) <> "]"

instance Print Instr where
  textPrint (Mov dest src) | isMemDest dest src = "mov qword " <> textPrint dest <> ", " <> textPrint src
  textPrint (Mov dest src) = "mov " <> textPrint dest <> ", " <> textPrint src
  textPrint (Neg dest) = "neg " <> textPrint dest
  textPrint (Add dest src) | isMemDest dest src = "add qword " <> textPrint dest <> ", " <> textPrint src
  textPrint (Add dest src) = "add " <> textPrint dest <> ", " <> textPrint src
  textPrint (Sub dest src) | isMemDest dest src = "sub qword " <> textPrint dest <> ", " <> textPrint src
  textPrint (Sub dest src) = "sub " <> textPrint dest <> ", " <> textPrint src
  textPrint (Xor dest src) = "xor " <> textPrint dest <> ", " <> textPrint src
  textPrint (Call label) = "call " <> textPrint label
  textPrint (Push arg) = "push " <> textPrint arg
  textPrint (Pop arg) = "pop " <> textPrint arg
  textPrint Ret = "ret"
  textPrint (Jmp label) = "jmp " <> textPrint label

printFormatLabel :: Text
printFormatLabel = "printf_format"
