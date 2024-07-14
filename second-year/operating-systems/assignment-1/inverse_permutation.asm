;Implementation of a function :
;bool inverse_permutation(size_t n, int *p);
;That inverts a given permutation and returns true
;or does nothing to the array and returns false if 
;the presented data is not valid.
global  inverse_permutation
inverse_permutation:
  push   rbp
  xor    eax, eax             ;eax register will be used as a return value, set it to 0
  xor    ecx, ecx             ;rcx is used in loops, set it to 0 as well
  mov    edx, 0x7FFFFFFF+1    ;edx will be used to change the first bit of a 32-bit register to 0
                              ;edx = 0b01...1 in binary
  ;rsi is an array pointer, *p
  ;rdi is an array length, n
  ;check n
  test   edi, edi             ;length of *p should be > 0, if(n == 0) return false
  jz     fail
  cmp    rdi, rdx             ;n is a 64-bit integer, need to check if it's >MAX_INT+1=0b10..0
  dec    edx
  ja     fail 
;check values in the array
;in this part of the program, check if p is a valid permutation
;use the array as a hash-map. mark already used indices by switching the first bit
;if there are two repeating indices, cell with that index will be negative
check_val_loop:               ;do{
  mov    r8d, [rsi+4*rcx]     ;int k = p[ecx]
  and    r8d, edx             ;if(k < 0) k += MAX_INT + 1
  cmp    r8d, edi             ;if(k >= n) jump to fail_loop
  jae    fail_loop
  mov    r9b, [rsi+4*r8+3]    ;int c = p[k]
  cmp    r9b, 0               ;if(c < 0) jump to fail_loop
  jl     fail_loop
  or     r9b, 0x80            ;if(c < 0) c += MAX_INT + 1 
  mov    [rsi+4*r8+3], r9b    ;p[k] = c
  inc    ecx                  ;ecx++
  cmp    ecx, edi             ;}while(ecx < edi)
  jb     check_val_loop
;if the array passes the check, invert the permutation:
;iterate over each element of p
;for each cycle, invert it and make all the elements of it positive
;so that they are different from the ones yet not touched
invert_loop:                  ;do {
  cmp    ecx, 0
  je     success              ;if(ecx == 0) return true
  dec    ecx                  ;ecx--
  mov    r8d, [rsi+4*rcx]     ;int k = p[ecx]
  cmp    r8d, 0               ;if(k >= 0) continue
  jge    invert_loop 
  and    r8d, edx             ;k += MAX_INT + 1
  mov    r9d, r8d             ;int prev = p[ecx]
  mov    r10d, ecx            ;int current = ecx
;iterate through every element in same cycle with the current one
start_cycle: ;do{
  mov    r11d, [rsi+4*r9]     ;int temp = p[prev]
  and    r11d, edx            ;if(temp < 0) temp += MAX_INT + 1
  mov    [rsi+4*r9], r10d     ;p[prev] = current
  mov    r10d, r9d            ;current = prev 
  mov    r9d, r11d            ;prev = temp
  cmp    ecx, r10d            ;}while(current != ecx)
  jne    start_cycle
  jmp    invert_loop          ;}while(true)
success:
  inc    al                   ;return true
  pop    rbp
  retq
;if the data is not correct, discard all changes
;go over each changed element and make sure index it's pointing to is positive.
fail_loop:                    ;do {
  dec    ecx                  ;ecx--
  cmp    ecx, 0               ;if(ecx < 0) return false
  jl     fail
  mov    r8d, [rsi+4*rcx]     ;k = p[ecx]
  and    r8d, edx             ;if(k < 0) k += MAX_INT + 1
  mov    r9b, [rsi+4*r8+3]    ;t = p[k] 
  and    r9b, 0x7F            ;if(t < 0) t += MAX_INT + 1
  mov    [rsi+4*r8+3], r9b
  jmp    fail_loop            ;}while(true)
fail: 
  pop    rbp                  ;return false
  retq

