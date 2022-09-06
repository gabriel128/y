global main
section .text

main:
    push rbp
    mov  rbp, rsp
    add  rsp, -24
    call start
    call conclusion

start:
  mov qword [rbp-24],3 
  mov qword [rbp-16],3 
  mov rax,[rbp-16] 
  add rax,[rbp-24] 
  mov qword [rbp-8],rax 
  mov rax,[rbp-8] 
  ret

conclusion:
    mov rsp, rbp
    pop rbp
    ret