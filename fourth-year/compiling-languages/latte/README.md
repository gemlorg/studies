
```markdown
# README: Compiler Project

## Overview

This project implements a compiler, for the Latte language, the executale compiles the source code to x86 assembly, using llvm as an IR for better optimisation. 

None of the extensions are currently implemented, but if they will be, it would be mentioned in the README file

## NEW FEATURES
- **Function Inlining**: Works in the following way: checks which functions don't call any other functions and inlines them. afterwards, repeats untill the result is the same. Tests can be found in `tests/lattests/opt`
- **Classes**: Fields, methods, extensions, virtual functions.
- **Arrays**: On all basic types, as well as classes.
## Directory Structure

- **`src/`**: Contains the source code for the project, organized into key components:
  - `Common/`: Shared utilities and exceptions logic.
  - `Compiler/`: Main compiler logic.
  - `Compiler/IR`: Compiler to llvm.
  - `Compiler/Optimizer`: A module to perform optimalizations like function inlining or GCSE.
  - `Grammar/`: Grammar definition and parsers.
  - `Typechecker/`: Front-end.
  - `latc_llvm.hs`: The main file.
- **`tests/`**: Contains test scripts and programs for validating the compiler's functionality.

## Compilation and Testing

- **Build**: To compile the project, run:
  ```bash
  make
  ```
  This generates an executable `latc_llvm` in the project root. The executable compiles .lat files to .ll, .bc and executables, which allows for easier testing.

- **Testing**: The project includes a test suite that can be run with:
  ```bash
  make test
  ```
  The testing script (`test.sh`), as well as the tests, however, was taken from an external repository.



required haskell libs: 

    build-depends:   
          base          >=4.7   && <4.17
        , llvm-hs-pure  >=9.0   && <9.1
        , base >= 4.3 && <4.18
        , containers
        , mtl 
        , microlens
        , microlens-th
        , text
        , filepath
        , process
        , array
        , bytestring
        , tasty < 1.4
        , tasty-hunit
        , tasty-golden
        , tasty-hspec
        , prettyprinter 

currently implemented parts: 
front-end (4)
back-end LLVM (8) 
użycie rejestrów i phi zamiast alloc w LLVM - dodatkowo 2p
GCSE 5p
