.data
.text

.globl _start
_start:
    mov     x0, #4      /* status := 4 */
    mov     w8, #93     /* exit is syscall #93 */
    svc     #0          /* invoke syscall */