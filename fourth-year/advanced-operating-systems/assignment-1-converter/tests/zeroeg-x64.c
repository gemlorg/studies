#include <stdio.h>

extern int return_zero(void);
extern int is_positive(int);
extern int add_zero(int);

int real_main(void) {
  if (return_zero() != 0) {
    printf("FAIL: return_zero() != 0\n");
    return -1;
  }
  if (is_positive(10) != 1) {
    printf("FAIL: is_positive(10) != 1\n");
    return -1;
  }
  if (is_positive(0) != 0) {
    printf("FAIL: is_positive(0) != 0\n");
    return -1;
  }
  if (is_positive(-5) != 0) {
    printf("FAIL: is_positive(-5) != 0\n");
    return -1;
  }
  if (add_zero(42) != 42) {
    printf("FAIL: add_zero(42) != 42\n");
    return -1;
  }
  if (add_zero(-10) != -10) {
    printf("FAIL: add_zero(-10) != -10\n");
    return -1;
  }

  printf("OK\n");
  return 0;
}
