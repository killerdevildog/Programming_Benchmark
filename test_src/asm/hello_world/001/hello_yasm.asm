; YASM syntax (NASM-compatible)
section .rodata
    msg db 'Hello, World!', 10
    msg_len equ $ - msg

section .text
global _start

_start:
    ; write(1, msg, 14)
    mov rax, 1              ; syscall: write
    mov rdi, 1              ; fd: stdout
    lea rsi, [rel msg]      ; buf: msg
    mov rdx, msg_len        ; count: 14
    syscall
    
    ; exit(0)
    mov rax, 60             ; syscall: exit
    xor rdi, rdi            ; status: 0
    syscall
