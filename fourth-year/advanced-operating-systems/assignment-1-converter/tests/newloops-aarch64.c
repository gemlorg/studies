// Tests various loop structures using only allowed instructions (ADD, CMP, B,
// B.cond etc.)

// Simple count-up loop. Expected: CMP, B.GE (out), ADD, B (back)
int count_up_simple(int limit) {
  int sum = 0;
  // Use volatile to prevent excessive optimization away of the loop counter
  volatile int i = 0;
  while (i < limit) { // CMP w_i, w_limit; B.GE end_loop
    sum = sum + i;    // ADD w_sum, w_sum, w_i
    i = i + 1;        // ADD w_i, w_i, #1
                      // B loop_check
  }
  // end_loop:
  return sum;
}

// Count-up loop with a break condition. Expected: Additional CMP, B.EQ (break),
// B (out)
int count_up_break(int limit, int break_val) {
  int sum = 0;
  volatile int i = 0;
  while (i < limit) {     // CMP, B.GE loop_end
    if (i == break_val) { // CMP w_i, w_break_val; B.EQ break_label
      break;
    }
    sum = sum + i; // ADD
    i = i + 1;     // ADD
                   // B loop_check
  }
  // break_label: (jumped here from B.EQ)
  // loop_end:
  return sum;
}

// Count-up loop with a continue condition. Expected: Additional CMP, B.EQ
// (continue), B (to increment/check)
int count_up_continue(int limit, int skip_val) {
  int sum = 0;
  volatile int i = 0;
  while (i < limit) {    // CMP, B.GE loop_end
    if (i == skip_val) { // CMP w_i, w_skip_val; B.EQ continue_label
      // If equal, skip the sum += i part
      goto increment; // B increment (or compiler structure)
    }
    sum = sum + i; // ADD (skipped if i == skip_val)
  increment:
    i = i + 1; // ADD
               // B loop_check
    // continue_label: (jumped here from B.EQ, goes straight to increment/check)
    // B increment (explicitly or implicitly part of compiler structure)
  }
  // loop_end:
  return sum;
}

// Loop condition based on added value, not just counter.
int while_add_condition(int start, int increment, int target) {
  int current_val = start;
  int iterations = 0;
  // Loop while current_val < target
  while (current_val < target) { // CMP w_curr, w_target; B.GE loop_end
    current_val = current_val + increment; // ADD w_curr, w_curr, w_inc
    iterations = iterations + 1;           // ADD w_iter, w_iter, #1
                                           // B loop_check
  }
  // loop_end:
  return iterations;
}
