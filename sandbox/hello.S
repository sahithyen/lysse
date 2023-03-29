.text

.globl _start
_start:
    mov $60, %rax
    mov $20, %rdi
    syscall
