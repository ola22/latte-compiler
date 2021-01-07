bits 64
default rel

global _strconcat
global _strcmp
global _strcmpn
global _printInt
global _printString
global _readInt
global _readString
global _error
global main

extern strconcat_c
extern strcmp_c
extern strcmpn_c
extern printInt_c
extern printString_c
extern readInt_c
extern readString_c
extern error_c
extern _main





_strconcat:
    mov r12, rsp
    ; putting values in to registers 
    ; according to C calling convention
    lea rdx, [rsp + 16]
    mov rdi, [rdx]
    lea rdx, [rsp + 24]
    mov rsi, [rdx]
    ; aligning stack
    and rsp, 0xFFFFFFFFFFFF0000
    ; calling c_function
    call strconcat_c
    ; storeing returned value on stack
    ; according to latte calling convention
    mov rsp, r12
    lea r12, [rsp + 8]
    mov [r12], rax
    ret


_strcmp:
    mov r12, rsp
    ; putting values in to registers 
    ; according to C calling convention
    lea rdx, [rsp + 16]
    mov rdi, [rdx]
    lea rdx, [rsp + 24]
    mov rsi, [rdx]
    ; aligning stack
    and rsp, 0xFFFFFFFFFFFF0000
    ; calling c_function
    call strcmp_c
    ; storeing returned value on stack
    ; according to latte calling convention
    mov rsp, r12
    lea r12, [rsp + 8]
    mov [r12], rax
    ret


_strcmpn:
    mov r12, rsp
    ; putting values in to registers 
    ; according to C calling convention
    lea rdx, [rsp + 16]
    mov rdi, [rdx]
    lea rdx, [rsp + 24]
    mov rsi, [rdx]
    ; aligning stack
    and rsp, 0xFFFFFFFFFFFF0000
    ; calling c_function
    call strcmpn_c
    mov rsp, r12
    ; storeing returned value on stack
    ; according to latte calling convention
    lea r12, [rsp + 8]
    mov [r12], rax
    ret


_printInt:
    mov r12, rsp
    ; putting values in to registers 
    ; according to C calling convention
    lea rdx, [rsp + 16]
    mov rdi, [rdx]
    ; aligning stack
    and rsp, 0xFFFFFFFFFFFF0000
    ; calling c_function
    call printInt_c
    mov rsp, r12
    ret


_printString:
    mov r12, rsp
    ; putting values in to registers 
    ; according to C calling convention
    lea rdx, [rsp + 16]
    mov rdi, [rdx]
    ; aligning stack
    and rsp, 0xFFFFFFFFFFFF0000
    ; calling c_function
    call printString_c
    mov rsp, r12
    ret


_readInt:
    mov r12, rsp
    ; aligning stack
    and rsp, 0xFFFFFFFFFFFF0000
    call readInt_c
    mov rsp, r12
    ; storeing returned value on stack
    ; according to latte calling convention
    lea r12, [rsp + 8]
    mov [r12], rax
    ret


_readString:
    mov r12, rsp
    ; aligning stack
    and rsp, 0xFFFFFFFFFFFF0000
    ; calling c_function
    call readString_c
    mov rsp, r12
    ; storeing returned value on stack
    ; according to latte calling convention
    lea r12, [rsp + 8]
    mov [r12], rax
    ret


_error:
    ; aligning stack
    and rsp, 0xFFFFFFFFFFFF0000
    call error_c
    ret


main:
    ; space for returned value for c
    sub rsp, 8
    ; calling latte main
    call _main 
    pop rax
    ret