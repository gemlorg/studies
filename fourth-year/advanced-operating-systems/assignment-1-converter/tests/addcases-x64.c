#include <stdio.h>

// Case 1 Test Functions
extern int add_case1_reg(int d, int m);
extern int add_case1_imm(int d);

// Case 2 Test Function (Result check is sufficient)
extern int add_case2_reg(int d, int n);

// Case 3 Test Functions
extern long long add_case3_reg(long long n, long long m);
extern long long add_case3_imm(long long n);

// Relocation Case Test Functions
extern void init_array_for_reloc(int index, char val);
extern int add_reloc_access(int index);


int real_main(void) {
    // Test Case 1
    if (add_case1_reg(10, 5) != 15) { printf("FAIL add_case1_reg\n"); return 1; }
    if (add_case1_reg(10, -3) != 7) { printf("FAIL add_case1_reg neg\n"); return 1; }
    if (add_case1_imm(20) != 35) { printf("FAIL add_case1_imm\n"); return 1; }
    if (add_case1_imm(-10) != 5) { printf("FAIL add_case1_imm neg\n"); return 1; }

    // Test Case 2 (Result check)
    if (add_case2_reg(10, 5) != 15) { printf("FAIL add_case2_reg\n"); return 1; }
    if (add_case2_reg(10, -3) != 7) { printf("FAIL add_case2_reg neg\n"); return 1; }

    // Test Case 3
    if (add_case3_reg(100LL, 200LL) != 300LL) { printf("FAIL add_case3_reg\n"); return 1; }
    if (add_case3_reg(100LL, -50LL) != 50LL) { printf("FAIL add_case3_reg neg\n"); return 1; }
     if (add_case3_imm(1000LL) != 1025LL) { printf("FAIL add_case3_imm\n"); return 1; }
    if (add_case3_imm(-10LL) != 15LL) { printf("FAIL add_case3_imm neg\n"); return 1; }

    // Test Relocation Case
    init_array_for_reloc(10, 'A');
    init_array_for_reloc(25, 'Z');
    if (add_reloc_access(10) != 'A') { printf("FAIL add_reloc_access(10)\n"); return 1; }
    if (add_reloc_access(25) != 'Z') { printf("FAIL add_reloc_access(25)\n"); return 1; }

    printf("OK\n");
    return 0;
}
