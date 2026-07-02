#include <stdio.h>

int answer = 3;

extern int set_answer(void);

int real_main(void) {
	set_answer();
	if (answer != 42)
		return -1;
	printf("OK\n");
	return 0;
}
