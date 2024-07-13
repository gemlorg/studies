# Introduction

The Game of Life, created by John Conway, is a cellular automaton, a simulation of a world of cells.

The simulation is conducted on a board consisting of infinitely many rows and infinitely many columns.

The rows and columns of the board are numbered with signed integers.

Each cell on the board is in one of two states: alive or dead.

The overall state of all cells is called a generation.

We assume that a cell in row w and column k neighbors eight other cells, which have row numbers from w - 1 to w + 1 and column numbers from k - 1 to k + 1.

The simulation starts from a certain initial generation, based on which the next ones are calculated.

In the next generation, a cell will be alive if and only if:

- in the current generation, it is alive and has exactly two or three live neighbors, or
- in the current generation, it is dead and has exactly three live neighbors.

## Task

Write a program that simulates the Game of Life.

The program reads the description of the initial generation from the input. Then, in a loop, it shows a fragment of the board, after which it reads and executes the user's command.

The commands control the calculation of successive generations. They also specify the fragment of the board, further referred to as the window, whose contents are shown to the user.

The program is parameterized by two positive integers:

- ROWS: the number of rows in the window;
- COLUMNS: the number of columns in the window.

The values of these parameters are defined using symbolic constants, which can be specified with the -D compiler option.

The program code provides default values:

- ROWS has a value of 22;
- COLUMNS has a value of 80.

The position of the window on the board is determined by the position of its upper-left corner. If the upper-left corner of the window is in row w and column k, the window includes cells with row numbers from w to w + ROWS - 1 and column numbers from k to k + COLUMNS - 1.

Initially, the upper-left corner of the window is in row number 1 and column number 1.

## Data Format

The program's input consists of the description of the initial generation and a sequence of commands.

The generation description indicates the cells that are alive. It is a sequence of rows starting with the '/' (slash) character.

In the last row of the generation description, there is only the '/' character. In all other rows, after the '/' character, there is an integer, representing the row number of the board. After it, there is a non-empty sequence of integers in ascending order, representing the column numbers. Each of these numbers is preceded by a space.

A row in the form:

```plaintext
/w k1 .. kn
```

indicates that in row w of the board, the cells in columns k1, .., kn are alive.

In the generation description, the sequence of row numbers, i.e., numbers preceded by '/', is in ascending order.

After the description of the initial generation, the program's input contains a sequence of single-line commands.

The program recognizes the following commands:

- Terminate the program:
  A line with the '.' (dot) character.
  It stops the command-reading loop and ends the program.

- Calculate the N-th successive generation:
  A line with a positive integer N.
  It calculates the N-th successive generation, starting from the current one. Specifically, the command 1 calculates the next generation.

- Calculate the next generation:
  An empty line. It is equivalent to the command 1.

- Dump the state of the current generation:
  A line with the number 0 (zero).
  It generates a complete description of the current generation in the same format as the initial generation description read by the program.

- Shift the window:
  A line with two integers, w and k, separated by a space.
  It changes the coordinates of the upper-left corner of the window, placing it in row w and column k.

## Output Format

Before reading each command, the program shows the contents of the window, starting from the upper-left corner.

The window contents are represented by ROWS rows, each with a length of COLUMNS. After the last of them, there is a row containing a sequence of '=' (equal sign) characters of length COLUMNS.

A character in a window content row represents the state of a cell. A live cell is represented by the '0' (zero) character, and a dead cell is represented by the '.' (dot) character.

## Examples

The following examples are the results of running the program compiled with the command:

```sh
gcc @options -DROWS=22 -DCOLUMNS=80 task3.c -o task3
```

The task includes .in files with sample data and .out files with reference results.

For data in example1.in, the correct result is example1.out.
For data in example2.in, the correct result is example2.out.
For data in example3.in, the correct result is example3.out.

## Validation and Tests

Solutions undergo validation, initially checking compliance with the specification.

Validation tests the program on examples included in the task description.

Successful validation is a prerequisite for the program to be allowed for correctness testing. A program that fails validation receives a zero correctness score.
Validation and tests are conducted on the students' computer.
Programs are compiled with the command:

```sh
gcc @options ... name.c -o name
```

where the ellipsis can include -D options defining constants ROWS and COLUMNS, name.c is the source code file name, and the options file contains:

```sh
-std=c17
-pedantic
-Wall
-Wextra
-Wformat-security
-Wduplicated-cond
-Wfloat-equal
-Wshadow
-Wconversion
-Wjump-misses-init
-Wlogical-not-parentheses
-Wnull-dereference
-Wvla
-Werror
-fstack-protector-strong
-fsanitize=undefined
-fno-sanitize-recover
-g
-fno-omit-frame-pointer
-O1
```

The -std=c17 and -pedantic options ensure the code complies with the current C language standard.

The -Wall and -Wextra options make the compiler report detected issues.

Options -Wformat-security, -Wduplicated-cond, -Wfloat-equal, -Wshadow, -Wconversion, -Wjump-misses-init, -Wlogical-not-parentheses, -Wnull-dereference enable additional issue detection.

The -Wvla option considers the use of variable-length arrays as an issue.

The -Werror option instructs the compiler to treat issues as errors.

The -fstack-protector-strong option helps detect some stack memory access errors during program execution.

Options -fsanitize=undefined, -fno-sanitize-recover detect operations with undefined effects.

Options -g, -fno-omit-frame-pointer improve error messages quality.

The -O1 option enables optimizations, increasing the likelihood of errors revealing.

All listed compiler options are required. No additional options will be added.

Different versions of the gcc compiler may handle the same options differently. It is recommended to compile and test the solution on students' computer as described above before submission.

During validation and tests, the program name is run under Valgrind control with the command:

```sh
valgrind --leak-check=full -q --error-exitcode=1 ./name arguments
```

If Valgrind detects an error, even if the result is correct, the test is considered failed.

The -q option causes Valgrind to output only error messages.

The --leak-check=full option instructs Valgrind to check for memory leaks, among other things.

The --error-exitcode=1 option sets the program exit code if Valgrind detects an error.
A non-zero main() function result indicates a runtime error.
Correctness of the result is checked by redirecting the program input from the .in file and comparing the result, using the diff program, with the .out file, e.g.:

```sh
< example.in ./name | diff - example.out
```

The correctness score is binary. The result is considered correct if the diff program shows no difference from the reference result.

## Notes

Submit a .c text file with the C source code as the solution.
Assume the data is valid.
Assume each data line, including the last one, ends with a newline character '\n'.

Ensure the program output meets this condition as well.
Assume the values of constants ROWS and COLUMNS, if defined during compilation, are positive integers.
The solution should have a memory cost linear with respect to the number of live cells.
Assume the window coordinates and the coordinates of live cells are within the range of the int type, and even that they are greater than INT_MIN and less than INT_MAX.
Do not limit the number of live cells. You can only assume that it will be within the range of the int type and that the representation of live cells will fit in the program's dynamic memory.
During validation and correctness tests, the time limit will be set to 30 seconds.

A solution with a time cost of calculating the next generation that is linear concerning the number of live cells will certainly fit within the time limit for all tests. A less efficient program may have problems with this in some tests. However, we guarantee that there will be at least 6 correctness tests that algorithms with a quadratic cost of calculating the next generation should handle.

Solutions with complexity worse than quadratic may not even pass validation tests.
