#include <stdio.h>

struct Point {
  long long x;
  int y;
  int z; // Padding implicitly handled by struct layout
};

extern long long get_x(struct Point *p);
extern int get_y(struct Point *p);
extern void set_x(struct Point *p, long long val);
extern void set_y(struct Point *p, int val);
extern void copy_point(struct Point *dest, struct Point *src);

int real_main(void) {
  struct Point p1 = {10LL, 20, 30};
  struct Point p2 = {0LL, 0, 0};

  if (get_x(&p1) != 10LL) {
    printf("FAIL get_x\n");
    return 1;
  }
  if (get_y(&p1) != 20) {
    printf("FAIL get_y\n");
    return 1;
  }

  set_x(&p2, 100LL);
  set_y(&p2, 200);
  // z is untouched by set_x/set_y, check it remains 0 initially
  if (p2.x != 100LL || p2.y != 200 || p2.z != 0) {
    printf("FAIL set_x/set_y\n");
    return 1;
  }

  copy_point(&p2, &p1);
  if (p2.x != p1.x || p2.y != p1.y || p2.z != p1.z) {
    printf("FAIL copy_point\n");
    return 1;
  }
  if (p2.x != 10LL || p2.y != 20 || p2.z != 30) {
    printf("FAIL copy_point values\n");
    return 1;
  }

  printf("OK\n");
  return 0;
}
