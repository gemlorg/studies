// Test various conditional branches

int check_conditions(int a, int b) {
  // Test EQ (-> JE)
  if (a == b) {
    return 10;
  }

  // Test NE (-> JNE)
  if (a != b) {
    // Test GT (-> JG)
    if (a > b) {
      return 20;
    }
    // Test LT (-> JL)
    if (a < b) {
      return 30;
    }
  }

  // Test GE (-> JGE)
  if (a >= b) {
    // Test LE (-> JLE) - use a different value to ensure path
    if (b <= a) { // Same as a >= b, maybe compiler uses different branch?
      return 40;
    }
  }

  return 99; // Default case
}
