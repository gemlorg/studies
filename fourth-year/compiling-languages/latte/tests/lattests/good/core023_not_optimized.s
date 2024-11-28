.data


.text

.global main
.global foo

    
__default_destructor:
    movl 4(%esp), %eax
    addl $-8, %eax
    push %eax
    call free
    addl $4, %esp
    ret
    
__array_destructor:
    push %ebx
    movl 8(%esp), %eax
    push %eax
    push (%eax)
    xorl %ebx, %ebx
    jmp .array_dtor_for_loop_cond_0
.array_dtor_for_loop_0:
    movl 4(%esp), %eax
    movl (%eax,%ebx,4), %eax
    test %eax, %eax
    jz .array_dtor_for_loop_cond_0
.__INFO_decr_ref_cnt_entry_0:
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .array_dtor_for_loop_cond_0
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.__INFO_decr_ref_cnt_end_0:
.array_dtor_for_loop_cond_0:
    incl %ebx
    movl (%esp), %eax
    cmpl %eax, %ebx
    jle .array_dtor_for_loop_0
    addl $4, %esp
    addl $-8, (%esp)
    call free
    addl $4, %esp
    pop %ebx
    ret
    
.__INFO_fn_entry_0:
readString:
    call __read_string
.__INFO_ref_cnt_init_0:
    movl $1, -4(%eax)
    movl $__default_destructor, -8(%eax)
    ret
.__INFO_fn_end_0:
    
.__INFO_fn_entry_1:
main:
    push %ebp
    movl %esp, %ebp
    subl $56, %esp
    movl $1, %eax
    movl %eax, -4(%ebp)
    movl $2, %eax
    movl %eax, -8(%ebp)
    movl $1, %eax
    movl %eax, -12(%ebp)
    movl $2, %eax
    movl %eax, -16(%ebp)
    movl $1, %eax
    movl %eax, -20(%ebp)
    movl $2, %eax
    movl %eax, -24(%ebp)
    movl $1, %eax
    movl %eax, -28(%ebp)
    movl $2, %eax
    movl %eax, -32(%ebp)
    movl $1, %eax
    movl %eax, -36(%ebp)
    movl $2, %eax
    movl %eax, -40(%ebp)
    movl $1, %eax
    movl %eax, -44(%ebp)
    movl $2, %eax
    movl %eax, -48(%ebp)
    movl $1, %eax
    movl %eax, -52(%ebp)
    movl $2, %eax
    movl %eax, -56(%ebp)
    movl -56(%ebp), %eax
    push %eax
    movl -52(%ebp), %eax
    push %eax
    movl -48(%ebp), %eax
    push %eax
    movl -44(%ebp), %eax
    push %eax
    movl -40(%ebp), %eax
    push %eax
    movl -36(%ebp), %eax
    push %eax
    movl -32(%ebp), %eax
    push %eax
    movl -28(%ebp), %eax
    push %eax
    movl -24(%ebp), %eax
    push %eax
    movl -20(%ebp), %eax
    push %eax
    movl -16(%ebp), %eax
    push %eax
    movl -12(%ebp), %eax
    push %eax
    movl -8(%ebp), %eax
    push %eax
    movl -4(%ebp), %eax
    push %eax
    call foo
    addl $56, %esp
    leave
    ret
.__INFO_fn_end_1:
    
.__INFO_fn_entry_2:
foo:
    push %ebp
    movl %esp, %ebp
    subl $4, %esp
    movl $10, %eax
    push %eax
    movl $2, %eax
    movl 8(%ebp), %ecx
    imul %ecx, %eax
    push %eax
    movl 12(%ebp), %eax
    movl $2, %ecx
    cltd
    idivl %ecx
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 16(%ebp), %eax
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 20(%ebp), %eax
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 24(%ebp), %eax
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 28(%ebp), %eax
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 32(%ebp), %eax
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 36(%ebp), %eax
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 40(%ebp), %eax
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 44(%ebp), %eax
    movl $2, %ecx
    cltd
    idivl %ecx
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 48(%ebp), %eax
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 52(%ebp), %eax
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 56(%ebp), %eax
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    push %eax
    movl 60(%ebp), %eax
    movl %eax, %ecx
    pop %eax
    addl %ecx, %eax
    pop %ecx
    cltd
    idivl %ecx
    movl %edx, %eax
    movl %eax, -4(%ebp)
    movl -4(%ebp), %eax
    push %eax
    call printInt
    addl $4, %esp
    movl -4(%ebp), %eax
    leave
    ret
.__INFO_fn_end_2:
