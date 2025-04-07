extern printf

global main

section .data
  printf_format: db '%d',10,0

section .text

main:
    push rbp
    mov  rbp, rsp
    add  rsp, -48
    call start
    call conclusion

start:
  mov qword [rbp-24], 9 
  mov qword [rbp-32], 7 
  mov rax, [rbp-32] 
  sub rax, 8 
  mov qword [rbp-16], rax 
  mov rax, [rbp-16] 
  add rax, [rbp-24] 
  mov qword [rbp-40], rax 
  mov qword [rbp-8], 0 
  mov rdi, printf_format 
  mov rsi, [rbp-40] 
  xor rax, rax 
  call printf WRT ..plt 
  mov rax, 0 
  ret

conclusion:
    mov rsp, rbp
    pop rbp
    ret