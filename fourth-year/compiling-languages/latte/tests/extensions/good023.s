_A__vTable:
    .int A.f

.data

.cstr0:
    .int 42
    .asciz "hejo"

.text

.global main
.global newA

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
    call newA
    push %eax
    movl (%eax), %eax
    movl 0(%eax), %eax
    call *%eax
    push %eax
    movl 4(%esp), %eax
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
    pop %eax
    addl $4, %esp
    movl $0, %eax
    leave
    ret
    
A.f:
    push %ebp
    movl %esp, %ebp
    push 8(%ebp)
    call __incr_ref_counter
    pop 8(%ebp)
    movl $.cstr0, %eax
    addl $4, %eax
    push %eax
    call __incr_ref_counter
    pop %eax
    push %eax
    movl 8(%ebp), %ecx
    leal 12(%ecx), %eax
    push %eax
    push 12(%ecx)
    call __incr_ref_counter
    addl $4, %esp
    pop %eax
    push %eax
    movl (%eax), %eax
    test %eax, %eax
    jz .maybe_decr_ref_cnt_end_1
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .maybe_decr_ref_cnt_end_1
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.maybe_decr_ref_cnt_end_1:
    pop %eax
    push %eax
    movl (%eax), %eax
    test %eax, %eax
    jz .maybe_decr_ref_cnt_end_2
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .maybe_decr_ref_cnt_end_2
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.maybe_decr_ref_cnt_end_2:
    pop %eax
    pop %ecx
    movl %ecx, (%eax)
    movl 8(%ebp), %ecx
    leal 12(%ecx), %eax
    push %eax
    push 12(%ecx)
    call __incr_ref_counter
    addl $4, %esp
    pop %eax
    movl (%eax), %eax
    push %eax
    movl 8(%ebp), %ecx
    leal 12(%ecx), %eax
    push %eax
    push 12(%ecx)
    call __incr_ref_counter
    addl $4, %esp
    pop %eax
    movl (%eax), %eax
    push %eax
    call __rstrconcat
    push %eax
    movl 4(%esp), %eax
    test %eax, %eax
    jz .maybe_decr_ref_cnt_end_3
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .maybe_decr_ref_cnt_end_3
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.maybe_decr_ref_cnt_end_3:
    movl 8(%esp), %eax
    test %eax, %eax
    jz .maybe_decr_ref_cnt_end_4
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .maybe_decr_ref_cnt_end_4
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.maybe_decr_ref_cnt_end_4:
    pop %eax
    addl $8, %esp
    movl $__default_destructor, -8(%eax)
    movl $1, -4(%eax)
    push %eax
    movl 8(%ebp), %ecx
    leal 12(%ecx), %eax
    push %eax
    push 12(%ecx)
    call __incr_ref_counter
    addl $4, %esp
    pop %eax
    push %eax
    movl (%eax), %eax
    test %eax, %eax
    jz .maybe_decr_ref_cnt_end_5
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .maybe_decr_ref_cnt_end_5
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.maybe_decr_ref_cnt_end_5:
    pop %eax
    push %eax
    movl (%eax), %eax
    test %eax, %eax
    jz .maybe_decr_ref_cnt_end_6
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .maybe_decr_ref_cnt_end_6
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.maybe_decr_ref_cnt_end_6:
    pop %eax
    pop %ecx
    movl %ecx, (%eax)
    movl $102, %eax
    push %eax
    call printInt
    addl $4, %esp
    movl 8(%ebp), %ecx
    leal 4(%ecx), %eax
    movl (%eax), %eax
    push %eax
    call printInt
    addl $4, %esp
    movl 8(%ebp), %eax
    test %eax, %eax
    jz .maybe_decr_ref_cnt_end_7
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .maybe_decr_ref_cnt_end_7
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.maybe_decr_ref_cnt_end_7:
    leave
    ret
    
__A.dtor:
    movl 4(%esp), %ecx
    movl 16(%ecx), %eax
    test %eax, %eax
    jz .class_dtor_field_check_end_0
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .class_dtor_field_check_end_0
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.class_dtor_field_check_end_0:
    movl 4(%esp), %ecx
    movl 12(%ecx), %eax
    test %eax, %eax
    jz .class_dtor_field_check_end_1
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .class_dtor_field_check_end_1
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.class_dtor_field_check_end_1:
    movl 4(%esp), %eax
    addl $-8, %eax
    push %eax
    call free
    addl $4, %esp
    ret
    
newA:
    push %ebp
    movl %esp, %ebp
    subl $4, %esp
    push $4
    push $7
    call calloc
    addl $8, %esp
    addl $8, %eax
    movl $1, -4(%eax)
    movl $__A.dtor, -8(%eax)
    movl $_A__vTable, (%eax)
    movl %eax, -4(%ebp)
    movl $42, %eax
    push %eax
    leal -4(%ebp), %eax
    push %eax
    push -4(%ebp)
    call __incr_ref_counter
    addl $4, %esp
    pop %eax
    movl (%eax), %eax
    push %eax
    leal 4(%eax), %eax
    pop %ebx
    push %eax
    decl -4(%ebx)
    movl -4(%ebx), %ecx
    test %ecx, %ecx
    jnz .decr_ref_cnt_emember_end_0
    push %ebx
    movl -8(%ebx), %eax
    call *%eax
    addl $4, %esp
.decr_ref_cnt_emember_end_0:
    pop %eax
    pop %ecx
    movl %ecx, (%eax)
    movl $101, %eax
    push %eax
    call printInt
    addl $4, %esp
    leal -4(%ebp), %eax
    push %eax
    push -4(%ebp)
    call __incr_ref_counter
    addl $4, %esp
    pop %eax
    movl (%eax), %eax
    push %eax
    push %eax
    call __incr_ref_counter
    addl $4, %esp
    movl -4(%ebp), %eax
    test %eax, %eax
    jz .maybe_decr_ref_cnt_end_8
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .maybe_decr_ref_cnt_end_8
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.maybe_decr_ref_cnt_end_8:
    pop %eax
    leave
    ret
    
    movl -4(%ebp), %eax
    test %eax, %eax
    jz .maybe_decr_ref_cnt_end_9
    decl -4(%eax)
    movl -4(%eax), %ecx
    test %ecx, %ecx
    jnz .maybe_decr_ref_cnt_end_9
    push %eax
    movl -8(%eax), %eax
    call *%eax
    addl $4, %esp
.maybe_decr_ref_cnt_end_9:
