int add_numbers(int a, int b) { return a + b; }

int multiply_numbers(int x, int y) {
  int result = 0;
  int i = 0;
  while (i < x) {
    result += y;
    i++;
  }
  return result;
}
