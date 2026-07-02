# Assignment 2: BPF compressibility analyzer

A set of Linux kernel changes that measure how well data written to files would compress, using
either the kernel's LZO implementation or a custom compression algorithm supplied as a BPF
program. The solution is delivered as a single patch against the lab kernel,
[`0001-add-compressor.patch`](0001-add-compressor.patch).

Original assignment statement:
<https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z2_ebpf/index.html>.

## Assignment

Extend the kernel so it can analyze the compressibility of data during write operations. A pair
of BPF programs is hooked into the syscall paths:

1. `open`/`openat`/`openat2`/`creat` — decide whether subsequent writes to the file should be
   checked for compressibility.
2. `write`/`pwrite`/`writev`/`pwritev2` — for flagged files, compress the written data and record
   the result (size before and after compression).

The BPF program has read access to a `compress_ctx` structure plus information about the file and
the data being written. Two new syscalls expose the recorded results:

```c
int get_compression_stats(int fd, size_t __user *bytes_written, size_t __user *bytes_after_compression);
int reset_compression_stats(int fd);
```

Technically this requires a new BPF program type `BPF_PROG_TYPE_COMPRESSOR` and attach type
`BPF_COMPRESSOR`, wired through the BPF verifier, syscall and trampoline machinery.

## Solution overview

The patch touches ~17 files (~470 insertions). The main pieces:

- **New BPF program/attach type** — `BPF_PROG_TYPE_COMPRESSOR` / `BPF_COMPRESSOR`, registered in
  `include/linux/bpf_types.h`, `include/uapi/linux/bpf.h`, and made verifiable in
  `kernel/bpf/verifier.c`.
- **Compressor core** — `kernel/bpf/bpf_compressor.c` (+ `include/linux/bpf_compressor.h`,
  `include/linux/compressor_hook_defs.h`) implements the hook, the LZO path, and the helpers
  exposed to BPF programs.
- **Syscall wiring** — two new syscalls (`get_compression_stats`, `reset_compression_stats`) in
  `arch/x86/entry/syscalls/syscall_64.tbl`, `include/uapi/asm-generic/unistd.h`,
  `include/linux/syscalls.h`.
- **VFS integration** — `fs/open.c` and `fs/read_write.c` invoke the hook on open/write, with
  per-file state carried in `include/linux/fs.h` / `fs/file_table.c`.

Design notes (from the author's write-up):

- A `COMPRESSOR_HOOK` mechanism modelled on the kernel's LSM hooks is used to attach the checker.
- The verify program runs *after* the original write completes: in `vfs_writev` all buffers are
  handled as a single `write_iter` operation, so the iov iterator is re-walked afterwards to run
  the compressor per buffer.
- The BPF context is embedded in a larger structure so helper functions can reach the full state
  while the BPF program only sees the `compress_ctx` fields.
- The trampoline change: rather than switching to `BPF_TRAMP_MODIFY_RETURN`, the fix was to relax
  the assumption that `tgt_prog` is non-null (a few added null checks), keeping `BPF_TRAMP_REPLACE`.

See `description.txt` for the author's original notes.

## Applying the patch

From the root of the lab kernel source tree:

```bash
git am 0001-add-compressor.patch      # or: patch -p1 < 0001-add-compressor.patch
```

Then rebuild the kernel as in the course labs.
