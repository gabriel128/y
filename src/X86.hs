module X86 where

-- reg ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
-- arg ::=  $int | %reg | int(%reg)
-- instr ::= addq  arg, arg | subq  arg, arg | negq  arg | movq  arg, arg | callq label | pushq arg | popq arg | retq
-- prog ::=  .globl main
--         main:  instr^{+}
