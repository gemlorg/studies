section .data 
global thread_conn
global thread_val

sync: times 2 * N dq N

section .text

global  core

extern put_value
extern get_value
core:
  push    rbp
  push    rbx  
  mov     rbx, rsp        ;preserve rsp
;rsi is the array pointer, *p
;rdi is the core, n
core_loop:
  pop     rax             ;load top stack value, return later if not used
  xor     ecx, ecx        
  mov     cl, [rsi]       ;load next instruction from input array  
  test    cl, cl          ;check if it's escape char already 
  jnz     check_star      ;if not, continue program
return:
  mov     rsp, rbx        ;otherwise, revert the stack
  pop     rbx             
  pop     rbp
  ret ;end program

;* – zdejmij dwie wartości ze stosu, oblicz ich iloczyn i wstaw wynik na stos 
check_star:
  cmp     cl, '*'
  jne     check_plus
  pop     rdx             ;pop second value from stack
  mul     rdx             ;multiply with first value(since we already popped to rax)
  push    rax             ;push result

;+ – zdejmij dwie wartości ze stosu, oblicz ich sumę i wstaw wynik na stos;
check_plus:
  cmp     cl, '+'
  jne     check_minus 
  add     [rsp], rax 

;- – zaneguj arytmetycznie wartość na wierzchołku stosu;
check_minus:
  cmp     cl, 45
  jne     check_integer
  xor     edx, edx        ;set edx to 0
  sub     rdx, rax        ;rdx = 0 - rax
  push    rdx             ;push result

;0 do 9 – wstaw na stos odpowiednio wartość 0 do 9;
check_integer:
  cmp     cl, '0'         
  jl      check_b
  cmp     cl, '9'
  jg      check_b
  mov     rdx, rcx        ;load rcx rdx 
  sub     rdx, 48         ;char to int
  push    rax             ;no need to use top stack value, put it back
  push    rdx             ;push result as well

;B – zdejmij wartość ze stosu, jeśli teraz na wierzchołku stosu 
;jest wartość różna od zera, potraktuj zdjętą wartość jako liczbę 
;w kodzie uzupełnieniowym do dwójki i przesuń się o tyle operacji;
check_b:
  cmp     cl, 'B'
  jne     check_d
  cmp     qword [rsp], 0  ;check that 0 isn't on top
  je      check_d         ;if it is, do nothing
  add     rsi, rax        ;otherwise, change instruction pointer accordingly

;D – wstaw na stos wartość z wierzchołka stosu, czyli zduplikuj wartość na wierzchu stosu;
check_d:
  cmp     cl, 'D'
  jne     check_e 
  push    rax             ;double the top stack value
  push    rax

;E – zamień miejscami dwie wartości na wierzchu stosu;
check_e:
  cmp     cl, 'E'
  jne     check_g
  pop     rdx             ;pop second stack value
  push    rax             ;top value is now second
  push    rdx             ;second is now top

;G – wstaw na stos wartość uzyskaną z wywołania (zaimplementowanej gdzieś 
;indziej w języku C) funkcji uint64_t get_value(uint64_t n);
check_g:
  cmp     cl, 'G'
  jne     check_p
  push    rax             ;preserve registers
  push    rdi
  push    rsi
  push    rcx 
  test    spl, 0b00001111 ;check if rsp % 16 ==0
  jnz     align_get       ;if isn't align first
  call    get_value       ;otherwise just call function
  jmp     restore_get     ;restore registers
align_get:
  sub     rsp, 8          ;align rsp to 16-byte boundary
  call    get_value       ;call function
  add     rsp, 8          ;restore rsp 
restore_get:
  pop     rcx             ;restore registers
  pop     rsi
  pop     rdi 
  push    rax             ;push result of get_value to stack

;P – zdejmij wartość ze stosu (oznaczmy ją przez w) i wywołaj(zaimplementowaną 
;gdzieś indziej w języku C) funkcję void put_value(uint64_t n, uint64_t w);
check_p:
  cmp     cl, 'P'
  jne     check_s
  push    rdi              ;preserve registers
  push    rsi
  push    rcx 
  mov     rsi, rax         ;top stack value is now second argument 
  test    spl, 0b00001111  ;check if rsp is aligned
  jnz     align_put        ;align if not
  call    put_value        ;call function
  jmp     restore_put      ;restore registers
align_put:
  sub     rsp, 8
  call    put_value
  add     rsp, 8
restore_put:
  pop     rcx              ;restore registers
  pop     rsi
  pop     rdi 

;S – synchronizuj rdzenie, zdejmij wartość ze
;stosu, potraktuj ją jako numer rdzenia m, czekaj na operację S
;rdzenia m ze zdjętym ze stosu numerem rdzenia n i
;zamień wartości na wierzchołkach stosów rdzeni m i n.
check_s:
  cmp     cl, 'S'
  jne     check_n
  lea     r8, sync[rel 0]  ;load relative adress of sync array
  pop     qword [r8+8*N+8*rdi] ;load value to sync array to switch
  mov     [r8+8*rdi], rax  ;sygnal that thread is ready switch
wait_sync:
  cmp     [r8+8*rax], rdi  ;check if other thread is ready
  jne     wait_sync        ;wait if not
  push    qword [r8+8*N+8*rax] ;push value from other thread
  mov     [r8+8*rax], rax  ;sygnal that the switch is ready
wait_after:
  cmp     [r8+8*rdi], rdi  ;wait for other thread to finish
  jne     wait_after

;n – wstaw na stos numer rdzenia;
check_n:
  cmp     cl, 110
  jne     loop_tail
  push    rax             ;push top stock value back
  push    rdi             ;push number of thread
loop_tail:
  inc     rsi             ;move onto next instruction
  jmp     core_loop

