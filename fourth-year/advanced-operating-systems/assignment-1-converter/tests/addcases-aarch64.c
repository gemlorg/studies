// Tests for specific ADD instruction conversion cases

// CASE 1: {op1} == {op2}. Expect AArch64: add Rd, Rd, Rm (or #imm)
// C: d = d + m; or d += m;

int add_case1_reg(int d, int m) {
    // Expect: add w0, w0, w1
    d = d + m;
    return d;
}

int add_case1_imm(int d) {
    // Expect: add w0, w0, #15
    d = d + 15;
    return d;
}

// CASE 2: {op1} == {op3}. Expect AArch64: add Rd, Rn, Rd
// C: d = n + d; (Semantically same as case 1, hard to guarantee specific asm form)
// We test the result, assuming the converter handles `add Rd, Rn, Rd` if generated.
int add_case2_reg(int d, int n) {
     // Might generate 'add w0, w1, w0' or just 'add w0, w0, w1'. Result is key.
    int result = n + d;
    return result;
}
// Case 2 immediate is not distinct from Case 1 immediate.


// CASE 3: {op1}, {op2}, {op3} different. Expect mov + add.
// AArch64: add Rd, Rn, Rm (or #imm) with Rd != Rn and Rd != Rm
// C: d = n + m; (where d, n, m likely assigned to different registers)
// Use volatile and distinct variables to encourage use of a separate destination register.

long long add_case3_reg(long long n, long long m) {
    // Aim: add x_tmp, x0, x1; then potentially mov x0, x_tmp for return
    // The conversion rule expects: mov {op1}, {op2}; add {op1}, {op3}
    // If AArch64 is `add x4, x0, x1`, x86 should be `mov r8, rdi; add r8, rsi`
    volatile long long d; // Try to prevent optimizing 'd' into x0 or x1 directly
    d = n + m;
    return d;
}

long long add_case3_imm(long long n) {
     // Aim: add x_tmp, x0, #25
     // Conversion rule expects: mov {op1}, {op2}; add {op1}, {op3} (imm)
     // If AArch64 is `add x4, x0, #25`, x86 should be `mov r8, rdi; add r8, 25`
    volatile long long d;
    d = n + 25;
    return d;
}


// Case: ADD with R_AARCH64_ADD_ABS_LO12_NC relocation
// Typically occurs when accessing array elements or struct members via address pointer.
// Need global data to trigger the relocation.

char my_global_array[100];

// ***** MODIFIED FUNCTION *****
// Change return type to char to avoid implicit sign extension during return
// that might trigger ldrsw or ldrsb.
char add_reloc_access(int index) {
    // Goal: Generate ADRP + ADD(reloc) + ADD(index) + LDRB/LDRSB
    // 1. ADRP x_tmp, my_global_array@PAGE
    // 2. ADD x_tmp, x_tmp, my_global_array@PAGEOFF   <-- This uses the relocation
    // 3. ADD x_idx, x_tmp, w0, SXTW #0  (Add index to base address)
    // 4. LDRB w0, [x_idx] (Load byte, zero-extended to w0)
    //    OR LDRSB w0, [x_idx] (Load byte, sign-extended to w0)
    // The 'char' return type makes LDRB more likely and avoids the need
    // for further extension that caused the problematic ldrsw.
    return my_global_array[index];
}

// Initialize the array element for testing
void init_array_for_reloc(int index, char val) {
     my_global_array[index] = val;
}
