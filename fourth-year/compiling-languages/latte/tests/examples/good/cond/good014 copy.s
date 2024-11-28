.data


.text

.global main
.global f

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
    leal (%eax,%ebx,4), %eax
    movl (%eax), %eax
    test %eax, %eax
    jz .array_dtor_for_loop_cond_0
.decr_ref_cnt_begin_0:
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .array_dtor_for_loop_cond_0
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.decr_ref_cnt_end_0:
.array_dtor_for_loop_cond_0:
    incl %ebx
    movl (%esp), %eax
    cmpl %eax, %ebx
    jle .array_dtor_for_loop_0
    pop %eax
    addl $-8, (%esp)
    call free
    pop %eax
    pop %ebx
    ret
    
.__fn_entry_0:
main:
    push %ebp
    movl %esp, %ebp
    movl $0, %eax
    push %eax
    call f
    addl $4, %esp
    movl $0, %eax
    leave
    ret
.__fn_end_0:
.__fn_entry_1:
f:
    push %ebp
    movl %esp, %ebp
    movl 8(%ebp), %eax
    push %eax
    movl $0, %eax
    pop %ecx
    cmpl %eax, %ecx
    jne .cond_else_0
.cond_then_0:
    movl $0, %eax
    leave
    ret
    
    jmp .cond_end_0
.cond_else_0:
    movl $1, %eax
    leave
    ret
.__fn_end_1:
