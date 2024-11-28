
```markdown
# README: Compiler Project

## Overview

This project implements a compiler, for the Latte language, the executale compiles the source code to x86 assembly, using llvm as an IR for better optimisation. 

None of the extensions are currently implemented, but if they will be, it would be mentioned in the README file
## Directory Structure

- **`src/`**: Contains the source code for the project, organized into key components:
  - `Common/`: Shared utilities and exceptions logic.
  - `Compiler/`: Main compiler logic.
  - `Grammar/`: Grammar definition and parsers.
  - `Typechecker/`: Front-end.
  - `latc_x86_64.hs`: The main file.
- **`tests/`**: Contains test scripts and programs for validating the compiler's functionality.

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
  The testing script (`test.sh`), as well as the tests, however, was taken from an external repository.


required haskell libs: 

    build-depends:    array
        , base >=4.7 && <5
        , containers
        , mtl >= 2.2
        , lens >= 4.19
        , text
        , filepath
        , process