module Nasm.Dsl where

import Data.Text (Text)
import Nasm.Data

-- $setup
-- >>> import Utils
-- >>> import Nasm.Data
-- >>> import Data.Text (Text, unpack)

-- | Mov Examples
-- >>> textPrint $ movrr Rbp Rsp
-- "mov rbp,rsp"
-- >>> textPrint $ movrm Rbp (deref R10 4)
-- "mov rbp,[r10+4]"
movrr :: Reg -> Reg -> Instr
movrr r1 r2 = Mov (RR r1 r2)

movrm :: Reg -> MemDeref -> Instr
movrm r1 mem = Mov (RM r1 mem)

-- | Sub Examples
-- >>> textPrint $ subri Rsp 16
-- "sub rsp,16"
-- >>> textPrint $ submi (deref Rbp 0) 3
-- "sub [rbp],3"
subri :: Reg -> Int -> Instr
subri r imm = Sub (RI r (Imm imm))

submi :: MemDeref -> Int -> Instr
submi mem imm = Sub (MI mem (Imm imm))

-- | Add Examples
-- >>> textPrint $ addri Rsp 16
-- "add rsp,16"
-- >>> textPrint $ addmi (deref Rbp 8) 3
-- "add [rbp+8],3"
addri :: Reg -> Int -> Instr
addri r imm = Add (RI r (Imm imm))

addmi :: MemDeref -> Int -> Instr
addmi mem imm = Add (MI mem (Imm imm))

-- | Neg Examples
-- >>> textPrint $ negr Rsp
-- "neg rsp"
-- >>> textPrint $ negm (deref Rbp (-4))
-- "neg [rbp-4]"
negr :: Reg -> Instr
negr r = Neg (RMr r)

negm :: MemDeref -> Instr
negm mem = Neg (RMm mem)

-- | Push Examples
-- >>> textPrint $ pushi 3
-- "push 3"
-- >>> textPrint $ pushm (deref Rsp 0)
-- "push [rsp]"
-- >>> textPrint $ pushr Rsp
-- "push rsp"
pushi :: Int -> Instr
pushi imm = Push (ArgI $ Imm imm)

pushr :: Reg -> Instr
pushr r = Push (ArgR r)

pushm :: MemDeref -> Instr
pushm mem = Push (ArgM mem)

-- | Pop Examples
-- >>> textPrint $ popi 3
-- "pop 3"
-- >>> textPrint $ popm (deref Rsp 0)
-- "pop [rsp]"
-- >>> textPrint $ popr Rsp
-- "pop rsp"
popi :: Int -> Instr
popi imm = Pop (ArgI $ Imm imm)

popr :: Reg -> Instr
popr r = Pop (ArgR r)

popm :: MemDeref -> Instr
popm mem = Pop (ArgM mem)

ret :: Instr
ret = Ret

jmp :: Text -> Instr
jmp = Jmp

deref :: Reg -> Offset -> MemDeref
deref = MemDeref
