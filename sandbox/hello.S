.text

.globl _start
_start:
    mov      $1, %rax
    mov      $1, %rdi
    lea      message(%rip), %rsi
    mov      $13, %rdx
    syscall

    mov $42, %edi
    mov $60, %eax
    syscall

message:
    .ascii  "Hello, world\n"
