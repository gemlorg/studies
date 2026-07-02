#include <stdio.h>
#define MAXDEPTH 4
extern int r_aarch64(int, int*);

int is_16_aligned(int* ptr) {
	return !(((long) ptr) & 0xf);
}

int r_x86_64(int depth) {
	int test __attribute__((aligned(16)));

	if (depth == MAXDEPTH) {
		return 0;
	} else
		return r_aarch64(depth + 1, &test);
}

int real_main(void) {
	if(r_x86_64(0) != 0)
		return -1;

	printf("OK\n");
	return 0;
}
