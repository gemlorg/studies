> Source: <https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z1_elf/index-en.html>

# Assignment 1: Binary file converter

Announcement date: 25.02.2025

Due date: 25.03.2025 (final due date 08.04.2025)

## Additional materials

- example of operation: [example.tar.xz](https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z1_elf/../_downloads/b9efe269662cfc97d5c3bad5e89e16e1/example.tar.xz)

- tests: [z1_test.tar.xz](https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z1_elf/../_downloads/7abcea282d431f77067d5be34ee183d2/z1_test.tar.xz)

## Introduction

The Arm architecture is common in smartphones and IoT. Its popularity is continuously increasing in other applications as well: computers (for example Apple M series) and cloud servers. Most computers still use the x86 architecture, though. Software compiled for one architecture cannot be run directly on the other - an extra layer that interprets the foreign architecture's instructions is required. Such mechanisms are provided by emulators with varying capabilities and performance, for example QEMU, which converts instructions between architectures just-in-time and Rosetta, which converts the whole code before running the program.

### Arm architecture naming

The naming of the 64-bit Arm architecture is somewhat confusing and often used inconsistently. According to the reference manual, the architecture is called the Arm architecture. It has several versions (the latest is Armv9) and three profiles: [A](https://developer.arm.com/documentation/ddi0487/latest/), R and M. AArch64 is a 64-bit execution mode introduced in Armv8-A; other sources also call it ARM64. AArch64 uses an instruction set called [A64](https://developer.arm.com/documentation/ddi0602/2024-12/?lang=en).

## Assignment

Write a converter of AArch64 `ET_REL` files into x86-64 `ET_REL` files.

## Assumptions

1.  The input to the conversion process is 1 binary file in `ET_REL` format and with `EM_AARCH64` architecture.

2.  The output of the conversion process shall be 1 binary file in `ET_REL` format and with `EM_X86_64` architecture.

3.  The `ET_REL` file can contain:

    - sections called `.note.gnu.property` or `*.eh_frame` (for example `.eh_frame`, `.rela.eh_frame`) - they shall be deleted (also from the section name table)

    - one section with symbols (of the type `SHT_SYMTAB`), and in it:

      - symbols of the type `STT_NOTYPE`, `STT_FUNC` and `STT_OBJECT`

        - for own (i.e. defined) symbols their value (and size in the case of `STT_FUNC`) shall be adjusted according to their location in the section of the output file

        - external (i.e. undefined) symbols shall be left unchanged

        - you may assume that there are no symbols defined inside functions

      - symbols of the type `STT_SECTION` and `STT_FILE` - they shall be left unchanged

    - sections with relocations (of the type `SHT_RELA`), and in them:

      - relocations of the type `R_AARCH64_CALL26`, `R_AARCH64_ADD_ABS_LO12_NC` and `R_AARCH64_ADR_PREL_PG_HI21` inside functions - they shall be converted according to the instruction conversion guidelines below

      - relocations of the type `R_AARCH64_ABS64` outside functions - they shall be converted into relocations of the type `R_X86_64_64`

      You may assume that there will not be multiple relocations pertaining to the same location in the same section.

    - other sections: every function in the section (according to the symbol table) shall be replaced with a function converted to x86-64 according to the guidelines below (if the section contains no functions, its content shall be left unchanged)

4.  You do *not* have to support the following functionality:

    - symbols in the `SHN_COMMON` "section" or other special sections

    - types of symbols and relocations other than the ones specified above

    - instructions and relocations in instructions other than the ones specified in function conversion guidelines

## Function conversion

### Prologue and epilogue

We assume that every AArch64 function begins with the following prologue:

    stp x29, x30, [sp, #-prologue_shift]!
    mov x29, sp

where `#-prologue_shift` is a displacement divisible by 16, and ends with the following epilogue:

    ldp x29, x30, [sp], #prologue_shift
    ret

We also assume that this epilogue is the only exit point of the function.

The prologue shall be converted to:

    push rbp
    mov rbp, rsp
    sub rsp, #prologue_shift

and the epilogue to:

    mov rax, rdi
    leave
    ret

In AArch64 the return address is not stored on the stack by the function call instruction, but it is saved in the `x30` register (also called `lr` - *link register*). `x29` (also called `fp` - *frame pointer*) is an analogue of `rbp`. The AArch64 prologue stores both these registers on the stack. The converted prologue has an effect similar to the AArch64 one (but for simplicity the converted prologue stores the return address and previous stack frame pointer at the bottom of the stack frame, while the AArch64 one stores them at the top). The epilogue, aside from operating symmetrically to the prologue, puts the function's result in the proper register (see the next section).

### Register mapping

The 64-bit AArch64 registers shall be mapped to x86-64 registers in the following way:

    # Caller-saved:
    x0 -> rdi # 1. argument
    x1 -> rsi # 2. argument
    x2 -> rdx # 3. argument
    x3 -> rcx # 4. argument
    x4 -> r8 # 5. argument
    x5 -> r9 # 6. argument
    x9 -> rax
    x10 -> r10
    # Callee-saved:
    x29 -> rbp
    x19 -> rbx
    x20 -> r12
    x21 -> r13
    x22 -> r14
    x23 -> r15
    sp -> rsp

The 32-bit registers, which have a `w` prefix instead of `x` (e.g. `w0`, `w23`), shall be mapped analogously to 32-bit x86-64 registers (e.g. `edi`, `r15d`). `sp` in AArch64 is not a general-purpose register, and thus can only be used in certain instructions.

Additionally, the special-purpose registers `xzr` and `wzr`, which are always equal to `0`, shall be mapped to an immediate `0` whenever they appear in an instruction.

We assume that only the registers specified above can be used. Note that this is a strong assumption, as AArch64 has 31 general-purpose registers (and `sp`). Since x86-64 has only 16, supporting all AArch64 general-purpose registers would require more management than just a simple mapping.

When converting instructions we will sometimes use an additional register for temporary values, denoted by `{tmp}`. This register shall be `r11` or `r11d` depending on the size of the AArch64 instruction operands.

Such a mapping preserves the caller-saved and callee-saved registers as well as the registers used for passing the first 6 arguments. Thanks to that, as long as all functions have at most 6 arguments (which we assume in this assignment), using the mapping in every instruction ensures that the correct calling convention is used. There is one difference, though: in AArch64, unlike x86-64, the same register is used for returning the function's result and passing an argument. Because of that, we have to put the result in the correct register in the epilogue.

### Relocations

In AArch64 all instructions have a size of 32 bits, thus they have no room for 32-bit values that could be relocated. Because of that there are different relocation types for different instructions, depending on which bits of the instruction are meant for the relocated value. The address of these relocations is always equal to the address of the instruction. Moreover, offsets in instructions (e.g. in branches) are calculated from the instruction's address. Thanks to that relative relocations in AArch64 do not need the adjustment present in x86-64 instructions such as `call`.

### Instruction conversion

In the following descrition `{op1}`, `{op2}` and `{op3}` refer to the subsequent operands of the AArch64 instruction, mapped as required if they are registers.

#### `ldr`` ``reg,`` ``[base,`` ``disp]` (load from memory)

The following code shall be generated:

    mov {op1}, {size_qualifier} [{op2.base} + {op2.disp}]

where `{size_qualifier}` is `qword`` ``ptr` if `{op1}` is 64-bit and `dword`` ``ptr` if it is 32-bit.

#### `str`` ``reg,`` ``[base,`` ``disp]` (store to memory)

The following code shall be generated:

    mov {size_qualifier} [{op2.base} + {op2.disp}], {op1}

where `{size_qualifier}` is defined like above.

#### `adrp`` ``reg,`` ``imm` (get page address)

We assume that the immediate (offset) has a relocation of type `R_AARCH64_ADR_PREL_PG_HI21`. The relocation shall be converted to a relocation of type `R_X86_64_PC32` and the following code shall be generated:

    lea {op1}, [rip + 0x7fffffff] # the displacement forces the assembler to use a 32-bit immediate; it is relocated
    and {op1}, ~0xfff # set 12 lowest bits to 0

#### `mov/cmp`` ``reg,`` ``reg/imm`

The following code shall be generated:

    {mnemonic} {op1}, {op2}

#### `add`` ``reg,`` ``reg,`` ``reg/imm`

1.  If `{op1}` is the same as `{op2}` (that is, it is the same register), the following code shall be generated:

        {add op3 to op1}

2.  Otherwise if `{op1}` is the same as `{op3}`, the following code shall be generated:

        {add op2 to op1}

3.  Otherwise the following code shall be generated:

        mov {op1}, {op2}
        {add op3 to op1}

Where the definition of `{add`` ``opy`` ``to`` ``opx}` is as follows:

1.  If `{opy}` is an immediate and has a relocation of type `R_AARCH64_ADD_ABS_LO12_NC`, the relocation shall be converted to a relocation of type `R_X86_64_32` and the following code shall be generated:

        mov {tmp}, 0x7fffffff # the immediate is relocated
        and {tmp}, 0xfff
        add {opx}, {tmp}

2.  Otherwise the following code shall be generated:

        add {opx}, {opy}

#### `bl`` ``imm` (function call)

We assume that the immediate (offset) has a relocation of type `R_AARCH64_CALL26`. The relocation shall be converted to a relocation of type `R_X86_64_PC32` and the following code shall be generated:

    call 0x7fffffff # the offset is relocated
    mov rdi, rax # put the return value in the register to which x0 maps

#### Branches (jumps)

The offset of each branch must be adjusted to the converted code - so that it branches to the instructions to which the original target instruction was converted. In the description of the generated code this adjustment is denoted by `{adjust(op1)}`. We assume that branches always happen within a function.

##### `b`` ``imm` (branch)

The following code shall be generated:

    jmp {adjust(op1)}

##### `b.cond`` ``imm` (conditional branch)

The following code shall be generated:

    j{cond} {adjust(op1)}

The mapping of the conditions in conditional branches is as follows:

    eq -> e
    ne -> ne
    hs -> ae
    lo -> b
    mi -> s
    pl -> ns
    vs -> o
    vc -> no
    hi -> a
    ls -> be
    ge -> ge
    lt -> l
    gt -> g
    le -> le

There exist AArch64 conditions without an x86-64 counterpart, but we assume that they will not be used.

## Solution format

As your solution provide an archive containing:

- any number of source files with the solution's code in C++, Rust or Python

- if the solution is written in a compiled language, a Makefile that builds the solution or the corresponding file from another reasonable build system (e.g. cmake)

- a readme file with a short description of the solution and instructions for compiling on the qemu image from the first lab

The solution (after compilation if required) shall provide an executable called `converter`. The program shall have the following interface:

    ./converter <input ET_REL file> <output ET_REL file>

The converter and the converted files will be run inside qemu with the image from the first lab. We recommend checking that your solution compiles in that image.

Hand in your solution via Moodle.

## Grading

You can obtain up to 10 points. The assignment is graded based on automated tests and code review.

## Hints

For disassembling we recommend the [Capstone](https://www.capstone-engine.org/) library, and for assembling - [Keystone](https://www.keystone-engine.org/). To install them in Python:

    pip install capstone keystone-engine

Their documentation is quite lacking (though some is provided on the linked websites), so referring to the source code of the Python module can be helpful.

Using Capstone, you can check the offset of the immediate or displacement in an instruction using `imm_offset` and `disp_offset`.

To inspect AArch64 binaries you can use the `aarch64-linux-gnu` binutils, e.g. `aarch64-linux-gnu-readelf`, `aarch64-linux-gnu-objdump`.

## Literature

Reading anything from the literature list is not required, but you may find some details there:

- [Arm Architecture Reference Manual for A-profile architecture](https://developer.arm.com/documentation/ddi0487/latest/)

- [Arm A-profile A64 Instruction Set Architecture](https://developer.arm.com/documentation/ddi0602/2024-12/?lang=en)

- [ELF for the Arm® 64-bit Architecture (AArch64)](https://github.com/ARM-software/abi-aa/blob/main/aaelf64/aaelf64.rst)
