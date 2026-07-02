// Test usage related to the zero register (wzr/xzr -> immediate 0)

// Compiler might optimize `mov w0, #0` to `mov w0, wzr`
int return_zero() { return 0; }

// Comparison with zero: cmp w0, #0 or cmp w0, wzr
int is_positive(int a) {
  if (a > 0) {
    return 1;
  } else {
    return 0; // Includes zero and negative
  }
}

// Load zero into a register and use it
int add_zero(int a) {
  int zero = 0; // Might use wzr
  return a + zero;
}
