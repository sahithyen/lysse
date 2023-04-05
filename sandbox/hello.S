.text

.globl _start
_start:
    lea      message(%rip), %rsi
    mov      $2, %rdi
    add      $0x7fffffff, %rdi
    mov      %rdi, (%rsi)

    push    %rbp
    call exit

    mov      $1, %rax
    mov      $1, %rdi
    mov      $1, %rdx
    syscall

exit:
    mov $0, %edi
    mov $60, %eax
    syscall
    ret

.data
message:
    .word  0
