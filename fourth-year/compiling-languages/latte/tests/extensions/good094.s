.data


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
    movl (%esp), %ebx
    movl (%eax,%ebx,4), %eax
    test %eax, %eax
    jz .label1
    push %eax
    leal -8(%eax), %eax
    call *%eax
.label1:
    incl (%esp)
    movl 4(%esp), %eax
    cmpl %eax, (%esp)
    jle .label0
.label2:
main:
    push %ebp
    movl %esp, %ebp
    subl $12, %esp
    movl $10, %eax
    push %eax
    addl $3, %eax
    push $4
    push %eax
    call calloc
    addl $8, %esp
    pop %ecx
    addl $8, %eax
    movl $__default_destructor, -8(%eax)
    movl %ecx, (%eax)
    movl %eax, -4(%ebp)
    push %eax
    call __incr_ref_counter
    addl $4, %esp
    movl $0, -8(%ebp)
    movl $0, %eax
    movl %eax, -12(%ebp)
    jmp .label4
.label3:
    movl $100, %eax
    push %eax
    addl $3, %eax
    push $4
    push %eax
    call calloc
    addl $8, %esp
    pop %ecx
    addl $8, %eax
    movl $__array_destructor, -8(%eax)
    movl %ecx, (%eax)
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
    movl -4(%eax), %edx
    test %edx, %edx
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
    leal -12(%ebp), %eax
    incl (%eax)
.label4:
    leal -12(%ebp), %eax
    movl (%eax), %eax
    push %eax
    movl $10, %eax
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
    movl $0, %eax
    push %eax
    movl -4(%ebp), %eax
    test %eax, %eax
    jz .label8
    decl -4(%eax)
    movl -4(%eax), %edx
    test %edx, %edx
    jnz .label8
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.label8:
    movl -8(%ebp), %eax
    test %eax, %eax
    jz .label9
    decl -4(%eax)
    movl -4(%eax), %edx
    test %edx, %edx
    jnz .label9
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.label9:
    pop %eax
    leave
    ret
