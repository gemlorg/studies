#include <stdio.h>

extern int fib(int);

int real_main(void) {
	if (fib(10) != 55)
		return -1;
	printf("OK\n");
	return 0;
}
