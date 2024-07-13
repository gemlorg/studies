# Introduction

The rectangular board for the single-player game SameGame is divided into cells, organized into rows and columns.

Each cell is either empty or contains a block of a specific type.

The player removes groups of adjacent blocks of the same type from the board.

Removing a group of blocks is only possible if there are at least two blocks in the group.

After removing a group, the board is reorganized:

- Blocks fall into the empty spaces in the rows below.
- Columns with any blocks are shifted left to occupy the empty columns.

The game is parameterized by three positive integers:

- ROWS: the number of rows on the board;
- COLUMNS: the number of columns on the board;
- TYPES: the number of types of blocks.

## Task

Write a program that executes a single block removal command issued by a player in SameGame.

The game parameters ROWS, COLUMNS, and TYPES are defined using symbolic constants, which can be specified with the -D compiler option.

The program code provides default values for these constants:

- ROWS has a value of 10;
- COLUMNS has a value of 15;
- TYPES has a value of 4.

The program is invoked with two arguments - the coordinates of the cell chosen by the player. The first argument is the row number, starting from 0. The second argument is the column number, starting from 0. The cell at row 0 and column 0 is in the top-left corner of the board.

The program's input data, read from the input, is the current state of the board.

The program's output, written to the output, is the state of the board after executing the player's command.

The board state specifies, for each cell, whether there is a block, and if so, what type of block it is.

A set G of blocks on the board is called a group if three conditions are met:

1. All blocks in set G are of the same type;
2. From each block in set G, one can move to any other block in the set by making only steps to direct neighbors in the row or column and moving exclusively on cells with blocks from set G;
3. The set G cannot be extended without breaking conditions 1 and 2.

It can be noted that each block uniquely determines the group to which it belongs.

The program:

1. Reads the board state;
2. Checks if the cell at the coordinates specified by the program arguments contains a block belonging to a group with at least two elements, and if so, removes all blocks of that group from the board;
3. Reorganizes the board;
4. Writes the result.

While reorganizing the board, the program:

- As long as there is at least one block directly above an empty cell in the same column, shifts the block down by one position;
- As long as there is at least one non-empty column directly to the right of an empty column, shifts all blocks in the non-empty column one position to the left.

If the cell at the coordinates specified by the program arguments is empty or contains a block whose group is a single element, the program's output is the same as the input.

## Data Format

The number of rows in the program's input is specified by the ROWS constant. Each row ends with a newline character '\n'.

The number of characters in each row is specified by the COLUMNS constant.

The data character specifies the content of a board cell:

- The '.' character indicates that the cell is empty;
- A decimal digit, from '0' to '0' + TYPES - 1, represents the type of block.

The board described by the data is organized. There is no block below which there is an empty cell. There is also no non-empty column to the left of which there is an empty column.

## Output Format

The format of the program's output is the same as the data format.

## Examples

The following examples are the results of running the program compiled with the command:

```sh
gcc @options -DROWS=10 -DCOLUMNS=15 -DTYPES=4 task2.c -o task2
```

The task includes .in files with sample data and .out files with reference results.

For data in example1_8_9.in, the correct result for the program invoked with arguments 8 and 9 is example1_8_9.out.
For data in example2_9_4.in, the correct result for the program invoked with arguments 9 and 4 is example2_9_4.out.
For data in example3_6_7.in, the correct result for the program invoked with arguments 6 and 7 is example3_6_7.out.

## Validation and Tests

Solutions undergo validation, initially checking compliance with the specification.

Validation tests the program on examples included in the task description.

Successful validation is a prerequisite for the program to be allowed for correctness testing. A program that fails validation receives a zero correctness score.
Validation and tests are conducted on the students' computer.
Programs are compiled with the command:

```sh
gcc @options ... name.c -o name
```

where the ellipsis can include -D options defining constants ROWS, COLUMNS, and TYPES, name.c is the source code file name, and the options file contains:

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
< example.in ./name arguments | diff - example.out
```

The correctness score is binary. The result is considered correct if the diff program shows no difference from the reference result.

## Notes and Tips

Submit a .c text file with the C source code as the solution.
Assume the data is valid. This also applies to the arguments with which the program is invoked.
Assume each data line, including the last one, ends with a newline character '\n'.

Ensure the program output meets this condition as well.
Assume the values of constants ROWS, COLUMNS, TYPES, if defined during compilation, are positive integers. Assume TYPES <= 10.
Assume the board representation fits in memory.
Recursion may help in solving the task.
The task description includes the samegame.c program. It implements, using the ncurses library, a user interface for the SameGame. The new board state is calculated using the task2 executable, which is the solution to the task.
