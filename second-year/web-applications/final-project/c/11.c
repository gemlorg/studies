#include <stdio.h>

int sub(int a, int b) {
  return a - b;
}
int main() {
  int a = 5;
  int b = 4;
  int c = sub(a, b);
  printf("%d\n", c);
  return 0;
}

