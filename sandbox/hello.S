.text

.globl _start
_start:
    mov     x0, #1      /* fd := STDOUT_FILENO */
    ldr     x1, =msg    /* count := len */
    ldr     x2, =len    /* count := len */
    mov     x8, #64     /* write is syscall #64 */
    svc     #0          /* invoke syscall */

    mov     x0, #0      /* status := 0 */
    mov     x8, #93     /* exit is syscall #93 */
    svc     #0          /* invoke syscall */

/* Data segment: define our message string and calculate its length. */
msg:
    .ascii        "hello, sandbox\n"
len = . - msg