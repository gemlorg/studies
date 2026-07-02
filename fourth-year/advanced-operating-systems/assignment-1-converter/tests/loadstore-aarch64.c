// Test focusing on LDR and STR instructions (reg, [base + imm])

struct Point {
  long long x;
  int y;
  int z; // Ensure alignment/padding considerations
};

// Read members using LDR rd, [xn, #imm]
long long get_x(struct Point *p) {
  // LDR x0, [x0, #0] (offset for x)
  return p->x;
}

int get_y(struct Point *p) {
  // LDR w0, [x0, #8] (offset for y)
  return p->y;
}

// Write members using STR rt, [xn, #imm]
void set_x(struct Point *p, long long val) {
  // STR x1, [x0, #0]
  p->x = val;
}

void set_y(struct Point *p, int val) {
  // STR w1, [x0, #8]
  p->y = val;
}

// Copy struct using LDR/STR sequences
void copy_point(struct Point *dest, struct Point *src) {
  // LDR x_tmp, [x1, #0] ; STR x_tmp, [x0, #0]
  dest->x = src->x;
  // LDR w_tmp, [x1, #8] ; STR w_tmp, [x0, #8]
  dest->y = src->y;
  // LDR w_tmp, [x1, #12] ; STR w_tmp, [x0, #12]
  dest->z = src->z;
}
