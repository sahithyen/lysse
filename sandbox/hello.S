.text

.globl _start
_start:
    mov $60, %rdi
    mov $60, %rax

    mov %rdi, %rax

    cmp %rdi, %rax
    jle exit1

exit0:
    mov $0, %rdi
    mov $60, %rax
    syscall

exit1:
    mov $1, %rdi
    mov $60, %rax
    syscall