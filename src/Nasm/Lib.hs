{-# LANGUAGE QuasiQuotes #-}

module Nasm.Lib where

-- reference https://www.cs.virginia.edu/~evans/cs216/guides/x86.html
-- Grammar
-- reg ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
-- arg ::=  $int | %reg | int(%reg)
-- instr ::= addq  arg, arg | subq  arg, arg | negq  arg | movq  arg, arg | callq label | pushq arg | popq arg | retq
-- prog ::=  .globl main
--         main:  instr^{+}

import Data.List (intersperse)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Word
import Nasm.Data
import NeatInterpolation
import Utils

--
programTemplate :: Text -> Word8 -> Text
programTemplate logic stackOffset =
  let offset = pack $ show stackOffset
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

printInstrs :: [Instr] -> Text
printInstrs instrs = T.concat $ intersperse " \n" (fmap textPrint instrs)
