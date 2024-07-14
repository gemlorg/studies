# Inverting Permutations

Implement a function in assembly language, callable from C:

```c
bool inverse_permutation(size_t n, int *p);
```

## Function Description

The function's arguments are a pointer `p` to a non-empty array of integers and the size of this array `n`. If the array pointed to by `p` contains a permutation of numbers from the range 0 to `n-1`, the function inverts this permutation in place and returns `true`. Otherwise, the function returns `false`, and the contents of the array pointed to by `p` remain unchanged after the function completes. The function should detect evidently incorrect values of `n` – see the usage example. However, it can be assumed that the pointer `p` is valid.

## Submission Instructions

Submit your solution as a file named `inverse_permutation.asm` in Moodle.

## Compilation

The solution will be compiled with the following command:

```sh
nasm -f elf64 -w+all -w+error -o inverse_permutation.o inverse_permutation.asm
```

The solution must compile in the computer lab.

## Usage Example

An example usage is provided in the file `inverse_permutation_example.c`. It can be compiled and linked with the solution using the following commands:

```sh
gcc -c -Wall -Wextra -std=c17 -O2 -o inverse_permutation_example.o inverse_permutation_example.c
gcc -z noexecstack -o inverse_permutation_example inverse_permutation_example.o inverse_permutation.o
```

## Evaluation

The conformity of the solution with the specification will be evaluated using automatic tests. Compliance with ABI rules, correct memory references, and memory usage will also be checked. Efforts should be made to minimize the memory footprint of the solution. The automatic test score will be reduced proportionally to the size of the additional memory used by the solution (sections .bss, .data, .rodata, stack, heap). Additionally, a threshold for the size of the .text section will be established. Exceeding this threshold will result in a proportional reduction of the score. Another criterion for automatic evaluation will be the execution speed. A solution that is too slow will not receive the maximum score. The score may be lowered for poor programming style. The final grade may also depend on a personal explanation of the program's details to the course instructor.
