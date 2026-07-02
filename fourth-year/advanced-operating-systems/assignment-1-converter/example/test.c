#include <sys/mman.h>
#include <stdio.h>

void pstr(const char *str) {
	printf("%s", str);
}

void pnum(int n) {
	printf("%d", n);
}

extern int f(int x, int y);

void g(void) {
	f(3, 4);
}

int real_main(void) {
	return f(15,7) + f(42, 3);
}

int main(void) {
	int res = real_main();
	printf("res: %d\n", res);
	g();
	return 0;
}
