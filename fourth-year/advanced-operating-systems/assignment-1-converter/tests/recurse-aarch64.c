extern int r_x86_64(int);
extern int is_16_aligned(int*);

int r_aarch64(int depth, int *verify) {
	int test __attribute__((aligned(16)));
	if (is_16_aligned(verify) != 1)
		return 1;

	/* just pass */
	return r_x86_64(depth);
}
