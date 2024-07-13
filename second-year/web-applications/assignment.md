# Web Application for 8-bit Processor Program Compilation

## Project Overview

The major tasks of this course are aimed at creating a web application that allows the compilation of programs for 8-bit processors. The final application should allow the input of a file written in C code (by uploading or editing), setting compiler options, and obtaining the compiled content of the program. It is assumed that the code to be compiled will be written in C, and the compilation will be done using the SDCC compiler.

## Task Descriptions

### Task 1: General Application View

The first task is to create a general view of the application, effectively a mockup that can be displayed in a browser. The overall format of the application page should match the provided diagram.

#### Application Layout

Ensure that the respective fields on the page are filled with sample data. Various programs intended for compilation under SDCC can be found in repositories for:

- 8051
- STM8
- Z80

The fields shown in the diagram are intended to have the following functionalities:

- **Menu Bar**: A place where typical application menus, such as File, Edit, Options, etc., will appear. At least these three menus should be visible in the mockup.
- **File Selection**: A place to select and insert program code files. This area should allow files to be placed in it and have a typical tree structure of directories.
- **Program Text**: A place where the full code of the program to be compiled will be displayed.
- **Code Fragment**: A place where the selected code fragment will be displayed. At later stages of the application, for example, highlighting an identifier in the program text area should display its code in the code fragment area.
- **Tabs (Tab1 - Tab4)**: Tabs with different contents. These tabs are intended to group various categories of compiler options. For example, one tab will allow selecting the processor for which the code will be compiled, another for options dependent on the selected processor, a third for the source code standard (e.g., C99 or C11), and a fourth for refining the generated code format (e.g., whether the stack should be external, whether auxiliary data for profiling should be generated, etc.).
- **Data Dependent on Tab**: This field should display values dependent on the selected tab. For example, after selecting the tab controlling the processor choice, this area should allow selecting one of the available processors.

### Data Model

Create a data model considering the following entities:

- **Catalog**: These entities store information about files and other catalogs. In addition to describing the relationships with other entities, they should have:

  - Name
  - Optional description
  - Creation date
  - Owner
  - Availability marker (false when the catalog is deleted, initially true)
  - Availability marker change date
  - Last content change date

- **File**: These entities store the source code of programs. The source code is assumed to be divided into meaningful sections. In addition to describing the relationships with other entities, they should have:

  - Name
  - Optional description
  - Creation date
  - Owner
  - Availability marker (false when the catalog is deleted, initially true)
  - Availability marker change date
  - Last content change date

- **File Section**: These entities contain significant parts of the file or comments. The section structure is complex - a section can have subsections. In addition to describing the relationships with other entities, they should have:
  - Optional name
  - Optional description
  - Creation date
  - Section start
  - Section end
  - Section type
  - Section status
  - Status data
  - Content

#### Section Types

An entity that defines the type of section content. The category defines how the section is handled by the application. The minimum set of categories includes: procedure, comment, compiler directives (#define, #pragma, etc.), variable declarations, assembler insert.

#### Section Status

An entity defining the section status; example statuses: compiles with warnings, compiles without warnings, does not compile.

#### Status Data

Additional data related to the status, e.g., compilation error, indicating which line the warning concerns, etc.

#### User

An entity that defines the application user. In addition to describing the relationships with other entities, they should have:

- Name
- Login
- Password

### Link-Button Interface and Page Templates

The application should provide the ability to use the SDCC compiler's features in the following way:

- **File Selection Section**: Displays the directory structure available in the database. This section should be implemented as an appropriate page template filled with data from the database.
- **Add File**: Allows adding a file to the database, dividing it into sections according to the specified categories (partially automatically, but also allowing manual sectioning). Operations should be available in a menu on the menu bar.
- **Add Catalog**: Allows adding a catalog to the database. This operation should be available in a menu on the menu bar.
- **Delete Item**: Allows deleting an item in the catalog (file, another catalog). The item should be marked as unavailable rather than completely removed from the database. This operation should be available in a menu on the menu bar and linked with a mechanism for selecting the file or catalog.
- **Code Fragment Section**: Displays the content of the `filename.asm` file obtained from the command:
  ```bash
  sdcc -S <filename>.c
  ```
  The resulting `filename.asm` file should be divided into ranges separated by dashed lines. Hovering over a section should highlight it. Separate highlighting for the section header and content should be provided. This should be ensured by an appropriate page template.
- **Tabs at the Bottom of the Screen**: Provide four tabs with the following contents:
  - **First Tab (STANDARD)**: Indicates the compiler standard (at least C89, C99, C11). Allows selecting one, and all compiler executions should adhere to the selected standard.
  - **Second Tab (OPTIMIZATIONS)**: Contains a list of available optimizations (at least 3). Allows specifying the selected set of optimizations.
  - **Third Tab (PROCESSOR)**: Contains a list of processor architectures available in SDCC (at least MCS51, Z80, and STM8). Allows selecting one, and all compiler executions should adhere to the selected processor.
  - **Fourth Tab (DEPENDENT)**: Contains compiler options dependent on the processor. For each selected processor, allows selecting three options specific to it (e.g., for MCS51 processor, allows choosing the target program model - small, medium, large, huge model programs).

### New Features

- **User Login Infrastructure**: Add functionality for user login. No need for user registration through the interface; adding new users can be done through the application admin interface. Files and catalogs added by a user must be stored in the database as belonging to them.
- **Enhanced User Experience**: Avoid reloading the page for each file operation and menu operation.
- **Manual Section Entry**: Allow manually entering a section encompassing the selected file fragment. Highlight the selected section in the code view.
- **Compilation Errors**: Display compiler messages in the Code Fragment section. Help the user by highlighting the source code line indicated by the compiler. Clicking on the message should highlight the corresponding source code line.

### Grading Criteria

- **User Entity**: 1 point
- **File and Catalog Entities**: 1 point
- **Section Entities**: 1 point

### Interface and Page Templates

- **File Selection**: Display directory structure from the database.
- **Add File and Catalog**: Provide options in the menu bar.
- **Delete Item**: Mark item as unavailable, available in the menu bar.
- **Code Fragment Section**: Display `filename.asm` content, highlight sections on hover.
- **Tabs**: Provide options for compiler standard, optimizations, processor, and dependent options.

### Enhancements

- **User Login**: Infrastructure for logging in users.
- **Enhanced UX**: Avoid page reloads for file and menu operations.
- **Manual Section Entry**: Allow manual section creation in the code view.
- **Compilation Errors**: Display compiler messages and highlight relevant source code lines.
