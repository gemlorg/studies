#include "multiargs.h"
extern void validate_neg(long, long, long long, long long);
extern void validate(int, long, long long, unsigned int, unsigned long, unsigned long long);
extern void validate_ptr(void*);

// This function returns int, because GCC seems to insert a nop before the epilogue of a void function
int check(void) {
	validate_neg(L1, -L1, L1, -L1);
	validate_ptr((void*) (1L<<31));
	validate(I1, L1, LL1, U1, UL1, ULL1);
	return 0;
}
