#include <stdio.h>

extern int add_numbers(int, int);
extern int multiply_numbers(int x, int y);

int real_main(void) {
  int sum_res = add_numbers(25, 17);
  int mul_res1 = multiply_numbers(5, 5);
  int mul_res2 = multiply_numbers(100, 0);

  if (sum_res != 42) {
    printf("got sum: %d", sum_res);
    return -1;
  }

  if (mul_res1 != 25)
    return -1;
  if (mul_res2 != 0)
    return -1;

  printf("OK\n");
  return 0;
}
