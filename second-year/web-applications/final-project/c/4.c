section .data

; System function codes
SYS_EXIT    equ 60
SYS_CREATE  equ 85
SYS_OPEN    equ 2
SYS_READ    equ 0
SYS_WRITE   equ 1
SYS_CLOSE   equ 3

; File flags
O_RDONLY    equ 0
S_IRUSR     equ 00400q
S_IWUSR     equ 00200q
S_IRGRP     equ 00040q
S_IROTH     equ 00004q

; Error codes
ERROR       equ 1
SUCCESS     equ 0

; Character codes
CAPS        equ 83
LOWS        equ 115

; Buffer sizes
RBUFF_SIZE  equ 0x10
WBUFF_SIZE  equ 0x12        ; important: must be dividable by 3

section .bss

readBuffer  resb RBUFF_SIZE
writeBuffer resb WBUFF_SIZE
inFileDesc  resq 1
outFileDesc resq 1

section .text

global _start

%macro scall4 4
    mov rax, %1
    mov rdi, %2
    mov rsi, %3
    mov rdx, %4
    syscall
%endmacro

%macro scall3 3
    mov rax, %1
    mov rdi, %2
    mov rsi, %3
    syscall
%endmacro

%macro scall2 2
    mov rax, %1
    mov rdi, %2
    syscall
%endmacro

%macro test_fail 0
    cmp rax, -4096
    ja close_file
%endmacro

%macro test_file 1
    cmp %1, 0               ; all file registers are set to 0 before 
    jz fail                 ; they receive file descriptor
%endmacro

_start:
    ; Counters
    xor r8, r8              ; write buffer counter 
    xor r9, r9              ; read buffer counter
    xor r10, r10            ; sequence counter
    ; Too many or too few arguments
    cmp qword [rsp], 3
    jnz fail
    
    ; Open in_file
    scall3 SYS_OPEN, [rsp + 16], O_RDONLY
    test_fail
    mov qword [inFileDesc], rax
    
    ; Check if out_file already exists by opening it
    scall3 SYS_OPEN, [rsp + 24], O_RDONLY
    cmp rax, 0
    jge close_file
    ; Create out_file
    scall3 SYS_CREATE, [rsp + 24], S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH
    test_fail
    mov [outFileDesc], rax
    jmp load_read_buff

write_before_exit:
    scall4 SYS_WRITE, [outFileDesc], writeBuffer, r9
    test_fail

close_file:
    test_file qword [inFileDesc]    ; if in_file is opened, close it
    scall2 SYS_CLOSE, [inFileDesc]
    test_file qword [outFileDesc]   ; if out_file is opened, close it and exit
    scall2 SYS_CLOSE, [outFileDesc]
    cmp rax, 0                      ; if rached EOF end with 0
    jz success                      ; otherwise with 1

fail:
    scall2 SYS_EXIT, ERROR

success:
    scall2 SYS_EXIT, SUCCESS

load_read_buff:
    scall4 SYS_READ, [inFileDesc], readBuffer, RBUFF_SIZE
    test_fail
    xor r8, r8

    ; if rax = 0 then EOF has been reached
    cmp rax, 0
    jnz read

    mov rcx, -1
    jmp write_ctr

read:
    ; r8 = index read from buffer
    mov cl, byte [readBuffer + r8]
    inc r8
    
    ; Check for s's and S's
    cmp cl, CAPS
    jz write
    cmp cl, LOWS
    jz write
    
    ; It isn't s or S
    inc r10
    cmp rax, r8
    jz load_read_buff
    jmp read

write:
    cmp r9, WBUFF_SIZE
    jnz write_ctr
    ; Write buffer to file and reset counter
    scall4 SYS_WRITE, [outFileDesc], writeBuffer, WBUFF_SIZE
    test_fail
    xor r9, r9

write_ctr:
    cmp r10, 0
    jz write_s
    ; Write byte counter in little-endian and then S or s
    mov byte [writeBuffer + r9], r10b
    shr r10, 8
    inc r9
    mov byte [writeBuffer + r9], r10b
    xor r10, r10
    inc r9

write_s:
    cmp rcx, -1
    jz write_before_exit
    mov byte [writeBuffer + r9], cl
    inc r9
    jmp read

; TODO
; implement custom buffer size and find optimal for 1GB test

