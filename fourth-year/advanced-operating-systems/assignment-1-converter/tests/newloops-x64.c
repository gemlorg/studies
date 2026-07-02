#include <stdio.h>

extern int count_up_simple(int limit);
extern int count_up_break(int limit, int break_val);
extern int count_up_continue(int limit, int skip_val);
extern int while_add_condition(int start, int increment, int target);

int main_count_up_simple(int limit) {
  int expected_sum = 0;
  for (int i = 0; i < limit; ++i) {
    expected_sum += i;
  }
  return expected_sum;
}

int main_count_up_break(int limit, int break_val) {
  int expected_sum = 0;
  for (int i = 0; i < limit; ++i) {
    if (i == break_val) {
      break;
    }
    expected_sum += i;
  }
  return expected_sum;
}

int main_count_up_continue(int limit, int skip_val) {
  int expected_sum = 0;
  for (int i = 0; i < limit; ++i) {
    if (i == skip_val) {
      continue;
    }
    expected_sum += i;
  }
  return expected_sum;
}

int main_while_add_condition(int start, int increment, int target) {
  int current_val = start;
  int expected_iterations = 0;
  while (current_val < target) {
    current_val += increment;
    expected_iterations++;
  }
  return expected_iterations;
}

int real_main(void) {
  // Test count_up_simple
  if (count_up_simple(10) != main_count_up_simple(10)) {
    printf("FAIL count_up_simple(10)\n");
    return 1;
  }
  if (count_up_simple(0) != main_count_up_simple(0)) {
    printf("FAIL count_up_simple(0)\n");
    return 1;
  }
  if (count_up_simple(1) != main_count_up_simple(1)) {
    printf("FAIL count_up_simple(1)\n");
    return 1;
  }

  // Test count_up_break
  if (count_up_break(10, 5) != main_count_up_break(10, 5)) {
    printf("FAIL count_up_break(10, 5)\n");
    return 1;
  }
  if (count_up_break(10, 0) != main_count_up_break(10, 0)) {
    printf("FAIL count_up_break(10, 0)\n");
    return 1;
  }
  if (count_up_break(10, 9) != main_count_up_break(10, 9)) {
    printf("FAIL count_up_break(10, 9)\n");
    return 1;
  }
  if (count_up_break(10, 10) != main_count_up_break(10, 10)) {
    printf("FAIL count_up_break(10, 10)\n");
    return 1;
  } // break immediately
  if (count_up_break(5, 10) != main_count_up_break(5, 10)) {
    printf("FAIL count_up_break(5, 10)\n");
    return 1;
  } // break val never reached

  // Test count_up_continue
  if (count_up_continue(10, 3) != main_count_up_continue(10, 3)) {
    printf("FAIL count_up_continue(10, 3)\n");
    return 1;
  }
  if (count_up_continue(10, 0) != main_count_up_continue(10, 0)) {
    printf("FAIL count_up_continue(10, 0)\n");
    return 1;
  }
  if (count_up_continue(10, 9) != main_count_up_continue(10, 9)) {
    printf("FAIL count_up_continue(10, 9)\n");
    return 1;
  }
  if (count_up_continue(5, 10) != main_count_up_continue(5, 10)) {
    printf("FAIL count_up_continue(5, 10)\n");
    return 1;
  } // skip val never reached

  // Test while_add_condition
  if (while_add_condition(0, 1, 10) != main_while_add_condition(0, 1, 10)) {
    printf("FAIL while_add_cond(0,1,10)\n");
    return 1;
  } // 10 iterations
  if (while_add_condition(0, 3, 10) != main_while_add_condition(0, 3, 10)) {
    printf("FAIL while_add_cond(0,3,10)\n");
    return 1;
  } // 4 iterations (0,3,6,9)
  if (while_add_condition(10, 1, 10) != main_while_add_condition(10, 1, 10)) {
    printf("FAIL while_add_cond(10,1,10)\n");
    return 1;
  } // 0 iterations
  if (while_add_condition(0, 5, 0) != main_while_add_condition(0, 5, 0)) {
    printf("FAIL while_add_cond(0,5,0)\n");
    return 1;
  } // 0 iterations

  printf("OK\n");
  return 0;
}
