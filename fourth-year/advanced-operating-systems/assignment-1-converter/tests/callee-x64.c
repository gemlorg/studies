#include <stdint.h>
#include <stdio.h>

extern long long use_x19(long long);

// Function to check if rbx was preserved by the CALLER
// We clobber rbx inside this function deliberately after saving it.
long long check_rbx_preserved(long long input_val) {
  long long original_rbx;
  long long result;
  long long clobbered_rbx;

  // Save rbx (callee-saved)
  __asm__ volatile("movq %%rbx, %0"
                   : "=r"(original_rbx) /* output */
                   :                    /* no inputs */
                   :                    /* no clobbers (we save it first) */
  );

  // Clobber rbx to make sure the called function isn't relying on it
  // being untouched by us *before* the call.
  __asm__ volatile("movq $0xdeadbeefcafebabe, %%rbx"
                   :        /* no outputs */
                   :        /* no inputs */
                   : "%rbx" /* clobbers rbx */
  );

  // Call the function under test
  result = use_x19(input_val);

  // Read rbx *after* the call
  __asm__ volatile("movq %%rbx, %0"
                   : "=r"(clobbered_rbx) /* output */
                   :                     /* no inputs */
                   :                     /* no clobbers */
  );

  // Restore original rbx (important!)
  __asm__ volatile("movq %0, %%rbx"
                   :                   /* no outputs */
                   : "r"(original_rbx) /* input */
                   : "%rbx"            /* clobbers rbx */
  );

  // Check 1: Did the function return the expected calculated value?
  if (result != input_val + 10) {
    printf("FAIL: use_x19(%lld) returned %lld, expected %lld\n", input_val,
           result, input_val + 10);
    return -1; // Indicate failure
  }

  // Check 2: Did the call to the *converted* use_x19 preserve the caller's rbx?
  // It should have, because rbx is callee-saved in x86-64. If the
  // AArch64 code used x19, the converter (or the original compiled code +
  // converter) needs to ensure the corresponding rbx is saved/restored. Note:
  // We expect clobbered_rbx to hold the value we put just before the call.
  if (clobbered_rbx != 0xdeadbeefcafebabe) {
    printf("FAIL: Caller's RBX was not preserved across call to use_x19!\n");
    printf("Expected RBX: 0x%llx, Got RBX: 0x%llx\n",
           (unsigned long long)0xdeadbeefcafebabe,
           (unsigned long long)clobbered_rbx);
    return -2; // Indicate RBX failure
  }

  return 0; // Indicate success
}

int real_main(void) {

  if (check_rbx_preserved(100) != 0) {
    return -1;
  }
  if (check_rbx_preserved(0) != 0) {
    return -1;
  }
  if (check_rbx_preserved(-50) != 0) {
    return -1;
  }

  printf("OK\n");
  return 0;
}
