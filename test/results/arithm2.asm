extern printf

global main

section .data
  printf_format: db '%d',10,0

section .text

main:
    push rbp
    mov  rbp, rsp
    add  rsp, -32
    call start
    call conclusion

start:
  mov qword [rbp-16], 9 
  mov qword [rbp-24], 100 
  mov rax, [rbp-24] 
  add rax, [rbp-16] 
  mov qword [rbp-32], rax 
  mov rdi, printf_format 
  mov rsi, [rbp-32] 
  xor rax, rax 
  call printf WRT ..plt 
  mov qword [rbp-8], 3 
  add qword [rbp-8], 3 
  mov rdi, printf_format 
  mov rsi, [rbp-8] 
  xor rax, rax 
  call printf WRT ..plt 
  mov rax, 0 
  ret

conclusion:
    mov rsp, rbp
    pop rbp
    ret