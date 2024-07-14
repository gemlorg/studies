# Prolog Assignment (10 points)

## Simple Program Analyzer - Introduction

We are analyzing simple sequential programs whose code is represented as a (Prolog) list of instructions. The system can run any number of processes with identical code. The task is to write a program in Prolog that checks the safety condition, i.e., checks whether there is an execution interleaving where at least two processes are in the critical section simultaneously.

### Specification

- **Constants**: Integers.
- **Variables**: Simple (integer type) and arrays with integer elements. Each array is one-dimensional and the size equals the number of processes in the system. Arrays are indexed from 0. All variables (including array elements) are initialized to zero. All variables are global, meaning they are accessible to all processes running in the system.
- Each process has access to a constant named `pid`, whose value is the process identifier, ranging from 0 to N-1, where N is the total number of processes in the system.

### Expressions (Arithmetic and Logical)

- **Arithmetic Expressions**:

  ```
  wyrArytm ::= wyrProste | wyrProste oper wyrProste
  wyrProste ::= liczba | zmienna
  zmienna ::= ident | array(ident, wyrArytm)
  oper ::= + | - | * | /
  ```

- **Logical Expressions**:

  ```
  wyrLogiczne ::= wyrProste operRel wyrProste
  operRel ::= < | = | <>
  ```

- All variable names (simple, array) are Prolog constants (not numbers), e.g., x, k, chce, 'A'.

### Instructions

- `assign(zmienna, wyrArytm)`: Assigns the value of the given expression to the specified variable (simple or array element). Proceeds to the next instruction.
- `goto(liczba)`: Unconditional jump to the instruction at the specified index.
- `condGoto(wyrLogiczne, liczba)`: Conditional jump to the instruction at the specified index if the logical expression is true; otherwise, proceeds to the next instruction.
- `sekcja`: Critical section. Proceeds to the next instruction.

Instructions are indexed starting from 1. Each process starts execution from the instruction at index 1.

### Assumptions

- All variables take values within a certain limited range (e.g., 0..N, where N is the number of processes), although this range is not explicitly provided and does not need to be checked. Therefore, the number of states of our system is always finite.
- We assume the correctness of program execution, meaning correct array references, correct arithmetic expression evaluation, and correct jumps (e.g., no division by zero, no out-of-bounds array access, no jumps to non-existent instructions).
- Each process operates in an infinite loop (the last instruction of each program is an unconditional jump).

### Task

Write a Prolog procedure `verify/2` invoked as follows:

```prolog
verify(N, Program),
```

where:

- `N`: Number of processes running in the system (>= 1),
- `Program`: Name of the file containing the program (constant).

The procedure should check the validity of the invocation arguments (e.g., the number of processes, the availability of the program file). You can assume the correctness of the input data, i.e., the program file itself.

For valid data, the program should output:

- Information about whether the safety condition is met or not.
- In case of a safety breach, print an example of an incorrect interleaving along with information about which processes are in the critical section (process indices).

### Reminder

An incorrect interleaving is a sequence of program instructions executed by processes in the system that results in two processes being in the critical section simultaneously. A process can enter the critical section if its program counter points to the `sekcja` instruction. If the program counter points to the instruction immediately after the `sekcja` instruction, it means the process has exited the critical section (i.e., it is safe).

For example, for a program consisting of the instructions:

```prolog
[assign(x, pid), sekcja, goto(1)]
```

an incorrect interleaving is the following sequence of instructions:

```
Process 1: 1
Process 0: 1
```

Process 1 executed the first instruction of the program, then process 0 executed the same instruction. The program counters of both processes are now equal to 2, which is the address of the `sekcja` instruction, so each process can enter (and does enter) the critical section.

### Specification of Auxiliary Predicates

In addition to defining the main predicate (`verify/2`), the program should contain definitions for the following predicates, with minor technical changes allowed (e.g., adding a parameter).

1. `initState(+Program, +N, -InitialState)`

- `Program`: Term representation of the program.
- `N`: Number of processes in the system.
- `InitialState`: Representation of the initial state.

Note: The program text should include a comment describing the adopted representation of the system state.

2. `step(+Program, +CurrentState, ?ProcessId, -NextState)`

- `Program`: Term representation of the program.
- `CurrentState`: Information about the system state (values of all variables and program counters of all processes).
- `NextState`: Information about the system state after executing the current instruction by the process with the specified ID.

### Program File Format

Text file format:

```prolog
variables(ListOfSimpleVariableNames).
arrays(ListOfArrayVariableNames).
program(ListOfInstructions).
```

All lists are provided in Prolog notation.

### Example Programs

1. Implementation of Peterson's algorithm in the defined language (left, in parentheses, instruction indices).

```prolog
(1) assign(array(chce, pid), 1)
(2) assign(k, pid)
(3) condGoto(array(chce, 1-pid) = 0, 5)
(4) condGoto(k = pid, 3)
(5) sekcja
(6) assign(array(chce, pid), 0)
(7) goto(1)
```

Representation of the above program (file `peterson.txt`):

```prolog
variables([k]).
arrays([chce]).
program([
  assign(array(chce, pid), 1),
  assign(k, pid),
  condGoto(array(chce, 1-pid) = 0, 5),
  condGoto(k = pid, 3),
  sekcja,
  assign(array(chce, pid), 0),
  goto(1)
]).
```

2. Very simple incorrect program (`unsafe.txt`).

```prolog
variables([x]).
arrays([]).
program([
  assign(x, pid),
  sekcja,
  goto(1)
]).
```

### Indicative Scoring

- 5 points - (correct, good) definition of the predicate `step/4`
- 2 points - binary information whether the system meets the safety condition
- 3 points - finding and printing an incorrect interleaving
- -1 point - lack of (complete) description of the chosen representation of the system state

### Important Additional Notes

1. Programs must run correctly under SICStus Prolog on the `students` computer. Programs not meeting this criterion will not be checked.
2. You may use standard Prolog predicates used in exercises (e.g., `member/2`, `append/3`) and the `lists` library (list operations). Load the library with the directive `:- ensure_loaded(library(lists))` at the beginning of the source file.
3. Do not use `findall/3` (or `bagof/3`, `setof/3`). Programs using any of these predicates will be graded on a scale of 0-4 points.
4. Optimization is not required, so you can use simpler (more expensive) data structures, e.g., Prolog lists.
5. You may use negation, cut, if-then-else constructs, the `if/3` predicate, etc.
6. The program should be formatted clearly, with each line not exceeding 80 characters. For example, the QuickSort algorithm formatting:

```prolog
qsort([], []).
qsort([X | L], S) :- % non-obstructive comment
  partition(L, X, M, W), % list division into sublists
  qsort(M, SM), % sorting sublists
  qsort(W, SW),
  append(SM, [X|SW], S). % merging results
```

7. The program should contain (brief, concise) comments describing the (declarative) meaning of important predicates and the adopted (basic) solutions.

### Submitting the Solution

The solution should consist of a single file named `<student_id>.pl` (e.g., `ab123456.pl`), submitted via Moodle. The first line of the file should contain a comment with the author's name (anonymous submissions are not read).

### Example Analysis Results

```prolog
?- verify(2, 'peterson.txt').
Program is correct (safe).

?- verify(2, 'peterson-bad0.txt').
Program is incorrect.
Incorrect interleaving:
Process 0: 1
Process 1: 1
Process 1: 2
Process 1: 3
Process 0: 2
Process 0: 3
Process 0: 4

Processes in the critical section: 1, 0.

?- verify(2, any).
Error: file named 'any' not found

?- verify(0, 'unsafe.txt').
Error: parameter

0 should be a number > 0
```
