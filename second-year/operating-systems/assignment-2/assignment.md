# Distributed Stack Machine

Implement a simulator for a distributed stack machine in x86_64 assembly. The machine consists of N cores, numbered from 0 to N−1, where N is a constant determined at compile time. The simulator will be used from C by launching N threads, each calling the function:

```c
uint64_t core(uint64_t n, char const *p);
```

## Function Description

The parameter `n` contains the core number. The parameter `p` is a pointer to an ASCIIZ string defining the computation that the core should perform. The computation consists of operations performed on a stack, which is initially empty. The string characters are interpreted as follows:

- `+` – pop two values from the stack, compute their sum, and push the result onto the stack;
- `*` – pop two values from the stack, compute their product, and push the result onto the stack;
- `-` – arithmetically negate the value at the top of the stack;
- `0` to `9` – push the corresponding value 0 to 9 onto the stack;
- `n` – push the core number onto the stack;
- `B` – pop a value from the stack; if the value at the top of the stack is now non-zero, treat the popped value as a two's complement number and skip that many operations;
- `C` – pop and discard the value from the stack;
- `D` – duplicate the value at the top of the stack;
- `E` – swap the two values at the top of the stack;
- `G` – push the value obtained from calling the C function `uint64_t get_value(uint64_t n);`
- `P` – pop a value (denoted by `w`) from the stack and call the C function `void put_value(uint64_t n, uint64_t w);`
- `S` – synchronize cores; pop a value from the stack, treat it as core number `m`, wait for core `m` to perform an S operation with the popped value as core number `n`, and swap the values at the tops of the stacks of cores `m` and `n`.

When a core finishes executing the computation, its result, which is the result of the `core` function, is the value at the top of the stack. All operations are performed on 64-bit numbers modulo \(2^{64}\).

You can assume the provided core number is valid. You can also assume that the computation is correct, meaning it contains only the described characters, ends with a null byte, does not try to access a value from an empty stack, and does not lead to deadlock. The behavior of a core for incorrect computations is undefined.

The hardware stack of the processor should be used as the stack for the described calculations. No libraries are allowed. Synchronization of cores, i.e., the S operation, should be implemented using some variant of a spin lock.

The result of the computation "1nS" is undefined and depends on the implementation, although treating the sequence of operations `n` and `S` as operation `C` seems reasonable.

Do not assume any upper limit on the value of N other than what is determined by the processor architecture and available memory.

In the specification of operations performed by the core, the terms "pop from the stack" and "push onto the stack" should be interpreted as definitions of the operations to be performed, not as a requirement to use the `pop` or `push` instructions.

## Submission Instructions

Submit your solution as a file named `core.asm` in Moodle.

## Compilation

The solution will be compiled with the following command:

```sh
nasm -DN=XXX -f elf64 -w+all -w+error -o core.o core.asm
```

where `XXX` specifies the value of the constant `N`. The solution must compile in the computer lab.

## Usage Example

An example usage is provided in the attached file `example.c`. It can be compiled with the following commands:

```sh
nasm -DN=2 -f elf64 -w+all -w+error -o core.o core.asm
gcc -c -Wall -Wextra -std=c17 -O2 -o example.o example.c
gcc -z noexecstack -o example core.o example.o -lpthread
```

## Evaluation

The conformity of the solution with the specification will be evaluated using automatic tests. Compliance with ABI rules, i.e., correct use of registers and processor stack, will be checked. Correct memory references and memory usage will also be checked. Efforts should be made to minimize the memory footprint of the solution. The automatic test score will be reduced proportionally to the size of the memory used (sections .text, .bss, .data, .rodata, stack, heap) – thresholds will be established, exceeding which will result in point deductions. The quality of the source code, including comments, will be evaluated. The code should be well-commented, which means, among other things, that each procedure should include information on what it does, how parameters are passed to it, how its result is returned, and which registers it modifies. The same applies to macros. It is good practice to separate functional blocks in the code with empty lines and comment on each block's functionality. All key or non-trivial lines inside procedures or macros also require comments. In assembly language, it is not an exaggeration to comment almost every line of code, but comments describing the obvious should be avoided.

Formal requirements specified in the task description, such as the correctness of the file name, will be evaluated. Code that does not compile will receive 0 points.
