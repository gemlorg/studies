> Source: <https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z2_ebpf/index.html>

# Assignment 2: BPF compressibility analyzer

Announcement date: 25.03.2025

Due date: 06.05.2025 (final due date 20.05.2025)

## Additional materials

- `.config` file: [CONFIG_Z2](https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z2_ebpf/../_downloads/a43cc3374a8cd679a3b6502b8fdd1042/CONFIG_Z2)

- tests: [z2-tst-public.tar.xz](https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z2_ebpf/../_downloads/4156146b265728c04618c7b74593daca/z2-tst-public.tar.xz)

## Introduction

Data compression allows reducing the size of stored data but requires additional CPU and memory usage. Moreover, some data compress better than others, and in extreme cases, compressed data may occupy more space than before compression. As a result, despite the implementation of convenient compression in some file systems (e.g., F2FS, btrfs, ZFS), compression is often not used.

One of the most important features of BPF technology is facilitating monitoring and observation of what happens in the operating system. A valuable piece of information that could be obtained by extending the existing BPF implementation in the Linux kernel is the compressibility of data written to the system. The goal of this assignment is to enable the operating system to collect such information.

## Assignment

Implement a system for analyzing data compressibility during write operations. The system should allow using the LZO compression implementation available in the Linux kernel to verify compressibility but also enable writing a custom compression algorithm as a BPF program or combining both methods.

To this end, new system calls should be introduced, as well as a way to hook a mechanism for compression verification into the kernel - a pair of BPF programs that will be executed on the following system calls respectively:

1.  `open`, `openat`, `openat2`, `creat` - to determine whether compression verification should be performed on subsequent write operations to the file

2.  `write`, `pwrite`, `writev`, `pwritev2` - for files that require verification, these calls should check the compression and store the analysis results

A BPF program installed like this should have read access to the `compression_ctx` structure described below, as well as information about the file and the currently written data.

The result of the verification, i.e., the size of the data before and after compression, should be recorded. To access or reset the recorded results, two new system calls should be implemented:

    int get_compression_stats(int fd, size_t __user * bytes_written, size_t __user * bytes_after_compression)
    int reset_compression_stats(int fd)

## Technical details

- Add a new BPF program type: `BPF_PROG_TYPE_COMPRESSOR` (with a number one higher than `BPF_PROG_TYPE_NETFILTER`).

- Add a new BPF attach type: `BPF_COMPRESSOR` (with a number one higher than `BPF_TRACE_KPROBE_SESSION`).

The data passed in the BPF call context should have the following format:

    struct compress_ctx {
        union {
            struct {
                loff_t offset;
                size_t size;
            };
            struct {
                kuid_t uid;
                kgid_t gid;
            };
        };
    };

Two new BPF hook points should be added:

- `int`` ``bpf_compressor_decide(struct`` ``compress_ctx`` ``*)` - if it returns a positive number, this indicates that the file should undergo compressibility verification.

- `int`` ``bpf_compressor_verify(struct`` ``compress_ctx`` ``*)` - performs compressibility verification; in case of an error, the syscall being executed should be aborted with `-EINVAL`

The newly added BPF program type should be attachable to these hooks (and only them) via `BPF_RAW_TRACEPOINT_OPEN`. There is no need to support `mmap`, `vmsplice`, `splice`, `tee`, or `sendfile` in this assignment.

New functions should be added and made available for the new BPF program type (and only it) to allow the BPF program to perform compressibility verification:

- `int`` ``bpf_copy_from_buffer(void`` ``*ctx,`` ``unsigned`` ``long`` ``offset,`` ``void`` ``*ptr,`` ``unsigned`` ``long`` ``size)` - copies data from the buffer in the context to the memory pointed to by `ptr`.

- `int`` ``bpf_lzo_compress(void`` ``*ctx)` - compresses the data from the buffer in the context and returns the size of the compressed data or a negative number in case of an error.

The new program type should have access to the following functions (and no others):

- the base set of common BPF functions

- the above new functions

- the existing functions `get_current_uid_gid` and `get_current_pid_tgid`

## Compiling the solution

Running the solution requires a properly configured Linux kernel with settings such as `CONFIG_DEBUG_INFO_BTF=y`. These settings were intentionally omitted from the configuration provided in previous materials because they increase the compilation time and resource requirements.

Due to this, it may be necessary to expand the virtual machine image by a few gigabytes (the `qemu-img`` ``resize` command might be helpful). The easiest way to test the solution is to use the configuration file provided in the additional materials section.

Compiling the tests requires the `vmlinux.h` file, which was not included with the tests. It can be generated e.g. using the `bpftool` program. No additional modifications to the provided `libbpf` library are required.

To compile the solution and tests inside the virtual machine, you must also install the `dwarves` and `clang` packages.

## Hints

A fix for infinite loops that was recently implemented in the BPF subsystem ([https://lore.kernel.org/bpf/20241015150207.70264-2-leon.hwang@linux.dev/T/](https://lore.kernel.org/bpf/20241015150207.70264-2-leon.hwang@linux.dev/T/)) unelegantly introduces an assumption that `bpf_tramp_prog_type`` ``==`` ``BPF_TRAMP_REPLACE` implies `tgt_prog`` ``!=`` ``NULL`. The solution must take this into account, otherwise the kernel will panic due to NULL dereferences.

During testing, an error introduced by the student may prevent the virtual machine from booting. If this happens, it is recommended to use the `-kernel`, `-hda`, and `-append`` ``"root=/dev/sda3"` options with previously copied working `vmlinuz` and `initrd` files ([https://qemu-project.gitlab.io/qemu/system/linuxboot.html](https://qemu-project.gitlab.io/qemu/system/linuxboot.html)). If you did not prepare these files beforehand, you can extract them from the base image used in the labs.

To compile the kernel using multiple CPU cores, remember to use the `-j` option in the `make` command and ensure that the virtual machine can utilize multiple cores (e.g., via the `-smp` option).

## Solution format

As the solution submit a package containing:

- a patch for the Linux kernel version 6.12.6 generated using `git`` ``format-patch`

- a short description of the solution
