# Advanced Operating Systems (ZSO)

Coursework for *Zaawansowane systemy operacyjne* (Advanced Operating Systems) at MIMUW,
2024/2025. The course covers ELF and the toolchain, Linux kernel internals and interfaces,
BPF, kernel modules, character devices and the PCI bus. Course materials:
<https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/>.

There were three large assignments:

1. **Binary file converter** ([`z1_elf`](https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z1_elf/index-en.html))
   — a `converter` that rewrites AArch64 `ET_REL` object files into x86-64 `ET_REL` files,
   translating sections, symbols, relocations, and instructions.
2. **BPF compressibility analyzer** ([`z2_ebpf`](https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z2_ebpf/index.html))
   — kernel changes adding a `BPF_PROG_TYPE_COMPRESSOR` program type and new syscalls to measure
   the compressibility of data written to files.
3. **Accelerator Device** ([`z3_driver`](https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z3_driver/index.html))
   — a Linux PCI character-device driver for the imaginary `acceldev` ONNX accelerator.

## Contents

- [`assignment-3-acceldev/`](assignment-3-acceldev/) — the `acceldev` PCI driver (assignment 3).

Assignments 1 and 2 are not included here.
