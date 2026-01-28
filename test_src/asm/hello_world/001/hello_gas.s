// GAS (GNU Assembler) AT&T syntax version
.section .rodata
msg:
    .ascii "Hello, World!\n"
    msg_len = . - msg

.section .text
.global _start

_start:
    # write(1, msg, 14)
    mov $1, %rax            # syscall: write
    mov $1, %rdi            # fd: stdout
    lea msg(%rip), %rsi     # buf: msg
    mov $msg_len, %rdx      # count: 14
    syscall
    
    # exit(0)
    mov $60, %rax           # syscall: exit
    xor %rdi, %rdi          # status: 0
    syscall
