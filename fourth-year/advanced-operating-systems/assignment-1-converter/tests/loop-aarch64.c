int fib(int n) {
	int a = 0;
	int b = 1;
    int i = 0;
	while (i < n) {
        int tmp = a + b;
        a = b;
        b = tmp;

        tmp = i;
        i = tmp + 1;
	}
	return a;
}
