
```markdown
# README: Compiler Project

## Overview

This project implements a compiler with a focus on modularity and extensibility. It is written in **Haskell** and structured to facilitate both front-end and back-end development. The implementation adheres to course requirements, targeting both **LLVM IR** and **x86_64 assembly** for code generation.

## Directory Structure

- **`src/`**: Contains the source code for the project, organized into key components:
  - `Common/`: Shared utilities and data structures.
  - `Compiler/`: Main compiler logic.
  - `Grammar/`: Grammar definitions and parsers.
  - `Typechecker/`: Semantic analysis for type checking.
  - `latc_x86_64.hs`: The entry point for the x86_64 back-end.
- **`tests/`**: Contains test scripts and programs for validating the compiler's functionality.
- **`Makefile`**: Build system for compiling the project and managing dependencies.

## Compilation and Testing

- **Build**: To compile the project, run:
  ```bash
  make
  ```
  This generates an executable `latc_x86_64` in the project root.

- **Testing**: The project includes a test suite that can be run with:
  ```bash
  make test
  ```
  The testing script (`test.sh`) is adapted from an external repository, with proper attribution included in the source files.

## Key Features

- **Front-End**: Implements lexical, syntactic, and semantic analysis using tools and libraries native to Haskell.
- **Back-End**:
  - **LLVM IR**: Generates `.ll` files for intermediate representation and optimizations.
  - **x86_64 Assembly**: Produces `.s` assembly files optimized for x86_64 architecture.
- **Optimizations**: The LLVM back-end incorporates optimizations such as register allocation with `phi` nodes, avoiding unnecessary memory operations.

## Submission and Compatibility

The project is designed to be portable and runs on lab machines. It adheres to the required submission format (TAR archive). Once built, the `latc_x86_64` executable can process programs written in the target language, outputting assembly or LLVM IR as appropriate.
```

You can copy and paste this Markdown into your README file. Let me know if further adjustments are needed!