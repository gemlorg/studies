.data

.cstr0:
    .int 42
    .asciz "abc"
.cstr2:
    .int 42
    .asciz "failure"
.cstr1:
    .int 42
    .asciz "it works"

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
    subl $8, %esp
    movl $.cstr0, %eax
    addl $4, %eax
    push %eax
    call __incr_ref_counter
    pop %eax
    movl %eax, -4(%ebp)
    movl $.cstr0, %eax
    addl $4, %eax
    push %eax
    call __incr_ref_counter
    pop %eax
    movl %eax, -8(%ebp)
    movl -4(%ebp), %eax
    push %eax
    push -4(%ebp)
    call __incr_ref_counter
    addl $4, %esp
    pop %eax
    push %eax
    movl -8(%ebp), %eax
    push %eax
    push -8(%ebp)
    call __incr_ref_counter
    addl $4, %esp
    pop %eax
    push %eax
    call __streq
    push %eax
.decr_ref_cnt_safe_begin_0:
    movl 4(%esp), %eax
    test %eax, %eax
    jz .decr_ref_cnt_safe_end_0
.decr_ref_cnt_begin_1:
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .decr_ref_cnt_safe_end_0
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.decr_ref_cnt_end_1:
.decr_ref_cnt_safe_end_0:
.decr_ref_cnt_safe_begin_1:
    movl 8(%esp), %eax
    test %eax, %eax
    jz .decr_ref_cnt_safe_end_1
.decr_ref_cnt_begin_2:
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .decr_ref_cnt_safe_end_1
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.decr_ref_cnt_end_2:
.decr_ref_cnt_safe_end_1:
    pop %eax
    addl $8, %esp
    test %eax, %eax
    jz .cond_else_0
.cond_then_0:
    movl $.cstr1, %eax
    addl $4, %eax
    push %eax
    call __incr_ref_counter
    pop %eax
    push %eax
    call printString
    push %eax
.decr_ref_cnt_safe_begin_2:
    movl 4(%esp), %eax
    test %eax, %eax
    jz .decr_ref_cnt_safe_end_2
.decr_ref_cnt_begin_3:
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .decr_ref_cnt_safe_end_2
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.decr_ref_cnt_end_3:
.decr_ref_cnt_safe_end_2:
    pop %eax
    addl $4, %esp
    jmp .cond_end_0
.cond_else_0:
    movl $.cstr2, %eax
    addl $4, %eax
    push %eax
    call __incr_ref_counter
    pop %eax
    push %eax
    call printString
    push %eax
.decr_ref_cnt_safe_begin_3:
    movl 4(%esp), %eax
    test %eax, %eax
    jz .decr_ref_cnt_safe_end_3
.decr_ref_cnt_begin_4:
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .decr_ref_cnt_safe_end_3
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.decr_ref_cnt_end_4:
.decr_ref_cnt_safe_end_3:
    pop %eax
    addl $4, %esp
.cond_end_0:
    movl $0, %eax
    push %eax
.decr_ref_cnt_safe_begin_4:
    movl -8(%ebp), %eax
    test %eax, %eax
    jz .decr_ref_cnt_safe_end_4
.decr_ref_cnt_begin_5:
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .decr_ref_cnt_safe_end_4
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.decr_ref_cnt_end_5:
.decr_ref_cnt_safe_end_4:
.decr_ref_cnt_safe_begin_5:
    movl -4(%ebp), %eax
    test %eax, %eax
    jz .decr_ref_cnt_safe_end_5
.decr_ref_cnt_begin_6:
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .decr_ref_cnt_safe_end_5
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.decr_ref_cnt_end_6:
.decr_ref_cnt_safe_end_5:
    pop %eax
    leave
    ret
    
.decr_ref_cnt_safe_begin_6:
    movl -8(%ebp), %eax
    test %eax, %eax
    jz .decr_ref_cnt_safe_end_6
.decr_ref_cnt_begin_7:
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .decr_ref_cnt_safe_end_6
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.decr_ref_cnt_end_7:
.decr_ref_cnt_safe_end_6:
.decr_ref_cnt_safe_begin_7:
    movl -4(%ebp), %eax
    test %eax, %eax
    jz .decr_ref_cnt_safe_end_7
.decr_ref_cnt_begin_8:
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .decr_ref_cnt_safe_end_7
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.decr_ref_cnt_end_8:
.decr_ref_cnt_safe_end_7:
.__fn_end_0:
