# Assignment 1: Binary File Converter

## Project Description

This project implements a binary file converter that translates AArch64 (ARM64) `ET_REL` (relocatable object) files into x86-64 `ET_REL` files. 


## Dependencies

To build and run this project, the following are required:

* **`pkg-config`**
* **`cmake`**
* **`make`** 
* **`g++`** .
* **Keystone Engine**
* **Capstone Engine**


## Build Instructions

These instructions assume you are running within the QEMU image environment provided for the course labs.

1.  **Install Dependencies :**
    All of the required libraries can be installed from source, as they are already present in the `deps` folder:
    ```bash
    ./install_deps.sh
    ```

2.  **Configure with CMake:**
   In the root dir:
    ```bash
    cmake .
    ```

3.  **Compile the Project:**
    ```bash
    make
    ```
    This will create the `converter` executable.

## Usage

Run the converter from the command line, providing the input ET_REL AArch64 object file path and the desired output x86-64 object file path.

```bash
./converter <input_aarch64_file.o> <output_x86_64_file.o>
