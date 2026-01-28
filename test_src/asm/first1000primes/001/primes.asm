; NASM x86-64 - Find first 1000 primes
section .data
    newline db 10

section .bss
    result resq 1

section .text
global _start

; Check if number in rdi is prime
; Returns 1 in rax if prime, 0 otherwise
is_prime:
    cmp rdi, 2
    jl .not_prime
    je .is_prime
    
    test rdi, 1
    jz .not_prime
    
    mov rcx, 3
.loop:
    mov rax, rcx
    mul rax
    cmp rax, rdi
    jg .is_prime
    
    mov rax, rdi
    xor rdx, rdx
    div rcx
    test rdx, rdx
    jz .not_prime
    
    add rcx, 2
    jmp .loop

.is_prime:
    mov rax, 1
    ret

.not_prime:
    xor rax, rax
    ret

_start:
    xor r12, r12        ; count = 0
    mov r13, 2          ; num = 2
    xor r14, r14        ; last_prime = 0

.count_loop:
    cmp r12, 1000
    jge .done
    
    mov rdi, r13
    call is_prime
    test rax, rax
    jz .not_prime_found
    
    mov r14, r13        ; last_prime = num
    inc r12             ; count++

.not_prime_found:
    inc r13             ; num++
    jmp .count_loop

.done:
    ; Convert last_prime to string and print
    mov rax, r14
    call print_number
    
    ; Print newline
    mov rax, 1
    mov rdi, 1
    lea rsi, [rel newline]
    mov rdx, 1
    syscall
    
    ; Exit
    mov rax, 60
    xor rdi, rdi
    syscall

; Print number in rax
print_number:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    
    mov rdi, rsp
    add rdi, 31
    mov byte [rdi], 0
    mov rcx, 10
    mov rbx, rax
    
.convert_loop:
    xor rdx, rdx
    mov rax, rbx
    div rcx
    add dl, '0'
    dec rdi
    mov [rdi], dl
    mov rbx, rax
    test rax, rax
    jnz .convert_loop
    
    ; Calculate length
    mov rax, rsp
    add rax, 31
    sub rax, rdi
    mov rdx, rax
    
    ; Print
    mov rax, 1
    push rdi
    mov rdi, 1
    pop rsi
    syscall
    
    leave
    ret
