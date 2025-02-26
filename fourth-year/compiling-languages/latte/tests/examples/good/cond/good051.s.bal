.data

.cstr8:
    .int 42
    .asciz "Hello"
.cstr9:
    .int 42
    .asciz "World"

.text

.global main

__default_destructor:
    movl 4(%esp), %eax
    push %eax
    addl $-8, (%esp)
    call free
    addl $4, %esp
    ret
    
__array_destructor:
    movl 4(%esp), %eax
    push %eax
    push (%eax)
    push $0
    jmp .label1
.label0:
    movl 8(%esp), %eax
    movl (%esp), %ecx
    leal (%eax,%ecx,4), %eax
    movl (%eax), %eax
    test %eax, %eax
    jz .label1
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .label1
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.label1:
    incl (%esp)
    movl 4(%esp), %eax
    cmpl %eax, (%esp)
    jle .label0
    pop %eax
    pop %eax
    addl $-8, (%esp)
    call free
    pop %eax
    ret
    
main:
    push %ebp
    movl %esp, %ebp
    subl $16, %esp
    movl $2, %eax
    movl %eax, -4(%ebp)
    movl $1, %eax
    movl %eax, -8(%ebp)
    movl $3, %eax
    movl %eax, -12(%ebp)
    movl $4, %eax
    movl %eax, -16(%ebp)
    leal -4(%ebp), %eax
    movl (%eax), %eax
    push %eax
    leal -8(%ebp), %eax
    movl (%eax), %eax
    pop %ecx
    cmpl %eax, %ecx
    jg .label5
    jmp .label6
.label6:
    leal -12(%ebp), %eax
    movl (%eax), %eax
    push %eax
    leal -4(%ebp), %eax
    movl (%eax), %eax
    pop %ecx
    cmpl %eax, %ecx
    jl .label5
    jmp .label3
.label5:
    leal -4(%ebp), %eax
    movl (%eax), %eax
    push %eax
    leal -16(%ebp), %eax
    movl (%eax), %eax
    pop %ecx
    cmpl %eax, %ecx
    jg .label7
    jmp .label3
.label7:
    leal -16(%ebp), %eax
    movl (%eax), %eax
    push %eax
    leal -4(%ebp), %eax
    movl (%eax), %eax
    pop %ecx
    cmpl %eax, %ecx
    jle .label2
    jmp .label3
.label2:
    movl $.cstr8, %eax
    addl $4, %eax
    push %eax
    call printString
    addl $4, %esp
    jmp .label4
.label3:
    movl $.cstr9, %eax
    addl $4, %eax
    push %eax
    call printString
    addl $4, %esp
.label4:
    movl $1, %eax
    push %eax
    pop %eax
    leave
    ret
    
