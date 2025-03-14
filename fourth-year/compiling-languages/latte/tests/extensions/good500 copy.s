.data


.text

.global main

__default_destructor:
    movl 4(%esp), %eax
    addl $-8, %eax
    push %eax
    call free
    addl $4, %esp
    ret
    
__array_destructor:
    movl 4(%esp), %eax
    push %eax
    push (%eax)
    push $0
    jmp .for_loop_cond_0
.for_loop_0:
    movl 8(%esp), %eax
    movl (%esp), %ecx
    leal (%eax,%ecx,4), %eax
    movl (%eax), %eax
    test %eax, %eax
    jz .for_loop_cond_0
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .for_loop_cond_0
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.for_loop_cond_0:
    incl (%esp)
    movl 4(%esp), %eax
    cmpl %eax, (%esp)
    jle .for_loop_0
    pop %eax
    pop %eax
    addl $-8, (%esp)
    call free
    pop %eax
    ret
    
main:
    push %ebp
    movl %esp, %ebp
    movl $5, %eax
    subl $4, %esp
    push %eax
    push $4
    push $4
    call calloc
    addl $8, %esp
    addl $8, %eax
    movl $1, -4(%eax)
    movl $__A.dtor, -8(%eax)
    movl %eax, 8(%esp) ; memo address
    leal 4(%eax), %eax ; address of the field
    pop %ecx           ; pop rval to reg
    movl %ecx, (%eax)  ; load under given address
    pop %ecx           ; get objects address that was memorized
    movl %ecx, %eax
    push %eax
    call printInt
    pop %eax
    test %eax, %eax
    jz .maybe_decr_ref_cnt_end_0
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .maybe_decr_ref_cnt_end_0
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.maybe_decr_ref_cnt_end_0:
    movl $0, %eax
    leave
    ret
    
__A.dtor:
    movl 4(%esp), %eax
    addl $-8, %eax
    push %eax
    call free
    addl $4, %esp
    ret
    
