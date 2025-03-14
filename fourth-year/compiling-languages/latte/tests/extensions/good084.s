.data

.cstr2:
    .int 42
    .asciz "a"

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
    subl $8, %esp
    movl $0, %eax
    movl %eax, -4(%ebp)
    movl $.cstr2, %eax
    addl $4, %eax
    movl %eax, -8(%ebp)
    push %eax
    call __incr_ref_counter
    addl $4, %esp
    jmp .label4
.label3:
    leal -8(%ebp), %eax
    movl (%eax), %eax
    push %eax
    leal -8(%ebp), %eax
    movl (%eax), %eax
    push %eax
    call __rstrconcat
    addl $8, %esp
    movl $__default_destructor, -8(%eax)
    movl $0, -4(%eax)
    push %eax
    movl %esp, %eax
    push %eax
    push (%eax)
    call __incr_ref_counter
    addl $4, %esp
    pop %eax
    leal -8(%ebp), %eax
    push %eax
    movl (%eax), %eax
    test %eax, %eax
    jz .label5
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .label5
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.label5:
    pop %eax
    movl %eax, %eax
    pop %ecx
    movl %ecx, (%eax)
    leal -4(%ebp), %eax
    incl (%eax)
.label4:
    leal -4(%ebp), %eax
    movl (%eax), %eax
    push %eax
    movl $30, %eax
    movl %eax, %ecx
    pop %eax
    cmpl %ecx, %eax
    jl .label7
    movl $0, %eax
    jmp .label6
.label7:
    movl $1, %eax
.label6:
    test %eax, %eax
    jnz .label3
    leal -8(%ebp), %eax
    movl (%eax), %eax
    push %eax
    call printString
    addl $4, %esp
    movl $0, %eax
    push %eax
    movl -8(%ebp), %eax
    test %eax, %eax
    jz .label8
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .label8
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.label8:
    pop %eax
    leave
    ret
