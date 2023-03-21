.text

.globl _start
_start:
    str     x0, [sp, #-16]! 

    ldr     x0, [sp], #16

    mov     x0, #0
    mov     x8, #93
    svc     #0
    
    ret
