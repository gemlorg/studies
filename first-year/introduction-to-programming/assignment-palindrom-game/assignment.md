# Introduction

Palindrome is a variant of the game known as Connect Four.

The game is played by two players, referred to as First and Second, on a vertically positioned rectangular board divided into rows and columns. Each player has their own type of pawn.

At the beginning of the game, the board is empty. The First player starts.

Players take turns making a move by placing their pawn on the board. The pawn is placed in the column chosen by the player, in the first free cell from the bottom of the board.

A player wins if their move creates a palindrome of a specified length with pawns in consecutive cells of the same row, column, or diagonal.

The game is parameterized by three positive integers:

- ROWS: the number of rows on the board,
- COLUMNS: the number of columns on the board,
- LENGTH: the length of the palindrome to be formed.

## Task

Write a program that allows two players to play Palindrome.

The values of the game parameters ROWS, COLUMNS, and LENGTH are defined using symbolic constants, which can be specified using the -D compiler option.

The program code provides default values for these constants:

- ROWS has a value of 8,
- COLUMNS has a value of 8,
- LENGTH has a value of 5.

The program, in a loop:

1. Writes a diagram of the current board state and indicates which player should make a move;
2. Reads the player's command;
3. If it reads a move command, it executes it;
4. If it reads a game termination command, it ends the execution.

The loop ends when:

- The program receives a game termination command, or
- One of the players wins.

If one of the players wins, the program, at the end of execution, writes a diagram of the final board state and informs who won.

## Data Format

The program's input consists of player commands. Each command occupies one line. All commands are valid. After the last command, there is any text, ignored by the program.

The only character in the line of a move command is the name of the column where the player wants to place a pawn. Columns are named by successive lowercase letters starting from 'a'.

A move command is valid if there is a free cell in the specified column.

The only character in the line of a game termination command is a period.

## Output Format

The program's output is a sequence of board diagrams. After each diagram, there is a line indicating who should make a move or who won.

A diagram describes a board cell with a character:

- `-` when the cell is empty;
- `1` when the cell contains a pawn of the First player;
- `2` when the cell contains a pawn of the Second player.

Each cell description character is preceded by a space.

Cell descriptions are grouped into rows, ordered from top to bottom. Cells in a row are ordered according to columns, starting from column 'a'.

After the last row of cells in the diagram, there is a row with column names. Each is preceded by a space.

The information about who should make a move is in a line with the player's name, 1 or 2, followed by a colon.

The information about who won is in a line with the player's name, 1 or 2, followed by an exclamation mark.

There are no characters in the program's output that are not mentioned above.

Each printed line, including the last one, ends with a newline character `\n`.

## Examples

The following examples are the results of running the program compiled with the command:

```sh
gcc @options -DROWS=8 -DCOLUMNS=8 -DLENGTH=5 task1.c -o task1
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

where the ellipsis can include -D options defining constants ROWS, COLUMNS, and LENGTH, name.c is the source code file name, and the options file contains:

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
valgrind --leak-check=full -q --error-exitcode=1 ./name
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

## Notes and Tips

Submit a .c text file with the C source code as the solution.
Assume the data is valid.
Assume each data line, including the last one, ends with a newline character '\n'.

Ensure the program output meets this condition as well.
Assume the values of constants ROWS, COLUMNS, LENGTH, if defined during compilation, are integers from 1 to 26, satisfying the condition LENGTH <= min(ROWS, COLUMNS).
In Linux, interacting with the program in the console, end-of-data is signaled by pressing Ctrl-D.
The tee command can help prepare test data. It transfers data from input to output while simultaneously saving a copy to a file named by the command argument.

The command:

```sh
tee test.in | ./name
```

runs the name program, passing input to it while saving to the test.in file what the user types.

The test on the same data can be repeated with the command:

```sh
< test.in ./name > test.out
```
