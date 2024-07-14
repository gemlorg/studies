# JiPP Assignment 2 - "Interpreter"

In Assignment 2, you are required to implement an interpreter for a programming language. You can earn a maximum of 30 points for this task. The evaluation will be based on both the scope of the project (the "size" and "difficulty" of the chosen language for implementation) and the quality of the solution.

## Schedule

The task is divided into three phases (the third phase is optional):

1. **Declaration of the Language for Implementation – by Tuesday, April 9, 2024.**
   Submit (via Moodle) a document containing a description of the chosen language for implementation. The file format can be PDF, plain text, or ZIP containing files of the mentioned types. Please name the files in the format `firstname_lastname.pdf`. The submission should include:

   - **Grammar of the language**: It can be provided in EBNF notation (especially recommended for those planning to use BNFC) or any reasonable form "on paper" (starting at a certain level of abstraction – do not define literals, identifiers, etc.).
   - **Several example programs** illustrating the planned syntactic constructs.
   - **Textual description of the language** emphasizing unusual constructs; note! do not describe obvious things. For example, if borrowing from well-known languages like Pascal, C, Haskell, just mention them without detailed descriptions.
   - **Filled-in feature table** (included in the task description) and the total number of points expected for the interpreter, assuming the entire provided functionality is correctly implemented (see much lower).

   Missing the deadline for this stage will result in a deduction of 8 points from the final grade, and an incomplete submission (e.g., missing grammar) will result in a deduction of up to 8 points.

   The evaluator will confirm (or not) the maximum grade that can be obtained for correctly implementing the declared language. The final scope of the project can still be modified in agreement with the evaluator.

2. **First Version of the Interpreter – by Tuesday, May 14.**
   Submit a working implementation of the interpreter. Submit the file `firstname_lastname.zip` with the content described below via Moodle. Additionally, the evaluator may request a personal presentation of the solution.

3. **Final Version of the Interpreter – by Tuesday, June 4.**
   Optionally, in agreement with the evaluator, implement additional functionalities to improve the grade. Note that fixing significant errors and omissions found by the evaluator may not fully compensate for the deducted points. A high grade cannot be expected if the initial submission is an "empty shell" and the solution is only completed in the second iteration.

## Implementation and Package Content

The interpreter should be implemented in Haskell. The solution should be submitted as a file `firstname_lastname.zip`, which, when unpacked, creates a directory `firstname_lastname`, where running the command `make` builds the working interpreter without any issues (on the students machine using the Haskell compiler version 9.0.2 installed there). The interpreter should be executable with the command `./interpreter program`, where `program` denotes the file with the program to be interpreted. If the language does not support standard input, the interpreter invoked without a parameter can read the program from standard input. The results should be printed to standard output, and error messages to standard error. By default, the interpreter should not print any additional diagnostic messages.

The solution should also include a README file with a general description of the solution and how to run it (especially if it's not obvious), an updated feature table, and example programs:

- In the `good` subdirectory, include examples of correct programs illustrating all the scored constructs of the language. If possible, tests should be named `nn-name` or `nn-mm-name`, where `nn` and `mm` are the numbers of constructs from one of the requirement lists for standard projects below, e.g., `06-13-array-passing.prg`.
- In the `bad` subdirectory, include examples of incorrect programs to illustrate the interpreter's behavior in exceptional situations, such as (but depending on the language, the list should be extended):
  - Syntax errors (not too many),
  - Unknown identifier, unknown function, etc.,
  - Wrong number of arguments,
  - Type errors and other statically detectable errors (if the solution supports static type checking),
  - Runtime errors (division by zero, array index out of bounds, etc.).

## Language

There are no preliminary restrictions on the language – it can be imperative, functional, object-oriented, logical, or mixed. We particularly value non-standard ideas, and for such "interesting" languages, evaluators may make individual decisions regarding the grade even if the language does not meet the standard project requirements below. However, languages whose originality is limited to fancy syntax will not be highly valued.

To help students make decisions and standardize evaluations, we present two lines of standard projects and the requirements for each grade level (assuming a correct and well-written implementation). The task includes feature tables; one of them should be filled out during the project declaration stage and updated during the language implementation stage, marking planned/implemented functionalities with a plus sign (+).

Of course, to obtain the full number of points at a given level, you should also implement naturally resulting combinations of these features, e.g., implementing variable passing and arrays should also include passing arrays by variable, etc.

The target grammar of the language should be as conflict-free as possible. If some conflicts cannot be easily avoided, they should be documented – provide examples of expressions presenting the conflict and the effects of the conflict resolution adopted by the used parser generator. Undocumented conflicts in the grammar may result in a lower grade.

It should be possible to include comments in the programs.

Due to the ease of error handling, we recommend using BNFC with the `--functor` option as the parser generator. This program is available at [BNFC](http://bnfc.digitalgrammars.com/) and also on the students machine in the directory `/home/students/inf/PUBLIC/MRJP/bin`. The recommended `--functor` option places the positions of the respective input file fragments in the abstract syntax tree. This greatly facilitates printing useful error messages, without which the interpreter cannot receive a high grade.

For those planning to write an interpreter for a statically typed language, we advise writing the type checker and interpreter in parallel or the type checker first. Writing the type checker at the end is quite ungrateful since most errors reported by the type checker are already reported by the interpreter (e.g., trying to divide a number by a string).

## Imperative Language

An imperative language, preferably with a C or Java-based syntax. In the absence of original ideas, you can model it on the Latte language [Latte](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2022/Latte/).

**For 15 points**:

1. At least three types of values: int, bool, and string (e.g., if 2+2 then \_ parses, but the expression has an incorrect type).
2. Literals, arithmetic, comparisons.
3. Variables, assignment operation.
4. Explicit value printing (instruction or built-in print procedure).
5. while, if (with and without else, can also be if _ elif _ else \_ endif syntax).
6. Functions or procedures (no nesting), recursion.
   One thing from the following list or something of comparable difficulty:
7. At least two parameter passing methods (by variable/by value/in/out),
8. "Read-only" variables and their use, e.g., in the implementation of Pascal-style for loops (for i = start to end - the control variable value cannot be changed inside the loop, the end value is calculated only once - before entering the loop).

**For 20 points**:
As above, plus: 9. Identifier shadowing with static binding (local and global variables or nested procedures/functions). 10. Runtime error handling, e.g., division by zero (can be a graceful message and interpreter termination). 11. Functions accepting and returning values of any supported types (i.e., not just procedures; however, only functions can be implemented – like in C).

**For up to 30 points according to the price list...**: 12. Static typing (i.e., always terminating type-checking phase before program execution) – 4 points, 13. Arbitrarily nested function/procedure definitions with correct static identifier binding (like in Pascal) – 2 points, 14. Records or arrays indexed by int or something à la lists – 1 point,
OR multidimensional arrays passed and assigned "by pointer" (like in Java), not "by copy" – 2 points, 15. Arbitrarily nested tuples with assignment like in Python (syntax as desired) – 2 points, 16. Loop-breaking operations while - break and continue – 1 point, 17. Functions as parameters, returning functions as results, closures à la JavaScript, anonymous functions – 4 points, 18. Generator procedures and syntax for their use (e.g., like in Python - yield and next statements, as well as for x in generator(...)) – 3 points.

## Functional Language

A functional language, preferably with SML/Caml or Haskell syntax (or Lisp/Scheme for simpler ones).

**For 20 points**:

1. At least two types of values in expressions: int and bool (e.g., if 2+2 then \_ parses, but the expression has an incorrect type).
2. Arithmetic, comparisons.
3. Conditional expression if.
4. Multi-argument functions, recursion.
5. Anonymous functions, partial application, higher-order functions, closures.
6. Runtime error handling, e.g., division by zero (can be a graceful message and interpreter termination).
7. Static identifier binding at any level of nested definitions.
   Lists

with: 8. Pattern matching [] | x:xs (syntax can be different from Haskell), 9. or a set of built-in operations: empty, head, tail, 10. Syntactic sugar for list literals, e.g., [1,2,3].

**For 25 points**:
As above, plus: 11. Lists of any type, including nested lists and lists of functions, 12. Or general recursive algebraic data types (like data in Haskell) with pattern matching. They can be monomorphic, and pattern matching can be single-level. 13. Static typing (i.e., always terminating type-checking phase before program execution). At this level, explicit type declarations can be required.

**For 30 points**:
As above, plus: 14. General polymorphic and recursive algebraic data types.
Ideally, definitions of List (syntax can be different from Haskell, syntactic sugar not required), Maybe, and Either types, and their use in example programs should be provided. 15. Arbitrarily nested patterns in pattern matching.

**Bonus up to 6 points**: 16. Polymorphic types in ML style (like Caml or Haskell without classes) with type inference algorithm. Syntax for declaring (type) abbreviations is not required – 4 points, 17. Checking completeness of pattern matching and warning for partial function definitions (requires arbitrarily nested pattern matching) – 2 points.
An interpreter implementing this algorithm can score more than 30 points.
Note! Scoring points in the second term for type inference is possible only if the interpreter in the first term included type checking (static typing) and was scored close to 30 points. Relying on "pulling" a poor interpreter with the type inference algorithm has usually led to disappointment in the past.
