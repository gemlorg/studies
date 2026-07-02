// Test using a register that maps to an x86 callee-saved register.
// x19 -> rbx
// x20 -> r12
// x21 -> r13
// x22 -> r14
// x23 -> r15
// Note: FIXED_REGS *prevents* use of many registers. x19-x23 are NOT fixed.

// Use x19 (maps to rbx, which is callee-saved in x86)
// The AArch64 function *itself* must preserve x19 according to AAPCS64
// if it modifies it. The converter needs to ensure the translated
// x86 code also respects the x86-64 ABI regarding rbx.
// The compiler (AArch64) should add instructions to save/restore x19
// if it's used and clobbered across calls, or just use it if safe.
// This test primarily checks if the register mapping works and doesn't
// cause unexpected issues.

long long use_x19(long long input) {
  // Use volatile to encourage the compiler to actually use the register
  // and not optimize it away completely. `register asm("x19")` might be
  // too restrictive or fragile.
  volatile long long temp_val;

  // Simple operation using a register likely to be x19 if available
  // We can't *force* x19 easily without inline asm, but this gives it a chance.
  // A better check is done from the caller side in x86.
  temp_val = input + 1; // Compiler *might* use x19 here if needed.

  // Let's try to *force* it with inline asm, hoping it works with the fixed
  // regs Store input + 5 into x19, then return x19.
  long long output;
  __asm__ volatile("add %0, %1, #5\n\t"
                   : "=r"(output) /* output */
                   : "r"(input)   /* input */
                   :              /* no clobbers specified */
  );

  // Let's specifically use x19 - this is more direct
  register long long x19_val asm("x19");
  x19_val = input + 10;

  // Return the value we put in x19.
  // The function epilogue (ldp x29, x30...) doesn't restore x19,
  // the compiler must add separate saves/restores if needed around calls.
  // We return it via x0 (-> rdi -> rax in epilogue).
  return x19_val;
}
