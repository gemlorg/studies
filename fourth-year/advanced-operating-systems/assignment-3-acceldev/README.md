# Assignment 3: Accelerator Device (`acceldev`)

A Linux PCI driver for a simplified imaginary ONNX-acceleration device, **Acceldev**,
emulated as a PCI device in a modified QEMU. The driver exposes each attached device as
a character device `/dev/acceldevX` and is built as a loadable kernel module `acceldev.ko`.

Original assignment statement:
<https://students.mimuw.edu.pl/ZSO/PUBLIC-SO/2024-2025/z3_driver/index.html>
(device specification: `z3_driver/device.html`).

## Assignment

Implement a kernel-mode PCI driver for the Acceldev device. For every attached device the
driver creates `/dev/acceldevX` (X numbered from 0). The character device supports:

- **`open`** — allocates a new device context (up to `ACCELDEV_MAX_CONTEXTS`), registered on
  the device. Commands are submitted per context.
- **`close`** — unregisters the context and frees its resources.
- **`ioctl(ACCELDEV_IOCTL_CREATE_BUFFER)`** — creates a *code* buffer (for user commands) or a
  *data* buffer (shared memory bound to a context slot). Buffers support `mmap`; their size must
  be `<= ACCELDEV_BUFFER_MAX_SIZE` (4 MiB). Closing a buffer waits for all runs scheduled on the
  context so far, unbinds it, and frees the DMA memory.
- **`ioctl(ACCELDEV_IOCTL_RUN)`** — schedules execution of commands from a code buffer on a
  context (via `ACCELDEV_DEVICE_CMD_TYPE_RUN`). Runs that don't fit in `CMD_MANUAL_FEED` are
  queued in the driver until they can be submitted. Returns `EIO` if the context previously
  errored.
- **`ioctl(ACCELDEV_IOCTL_WAIT)`** — waits for completion of a specific fence
  (`ACCELDEV_USER_CMD_TYPE_FENCE`) on a context. Returns `EIO` on context error, `EINTR` on
  signal.

User-submitted commands inside code buffers are validated by the device, not the driver; on
error the device raises an interrupt and marks the context's status as errored. ioctl arguments
(sizes, alignment, buffer types) are validated by the driver.

## Solution overview

The driver is split into small translation units sharing `acceldev_driver.h`:

| File | Responsibility |
| --- | --- |
| `acceldev_pci.c` | Module init/exit: char-device region, device class, PCI driver registration. |
| `acceldev_driver.c` | PCI `probe`/`remove`, IRQ handler, device restart on error, per-device state and the device slot table. |
| `acceldev_fops.c` | `/dev/acceldevX` file operations: `open` (context allocation), `release`, ioctl dispatch, and context error helpers. |
| `acceldev_ioctl.c` | The three ioctls: `CREATE_BUFFER`, `RUN`, `WAIT`. |
| `acceldev_buffer.c` | Buffer file operations: `mmap` with a `fault` handler backing pages, and buffer release. |
| `acceldev_executor.c` | A kernel worker thread draining a queue of pending runs into the device command feed. |
| `acceldev_cmds.c` | Low-level MMIO register access and device-command encoding (feed, bind/unbind slot, run, fence). |
| `acceldev_utils.c` | Buffer/slot manager and DMA page-table (`acceldev_pt`) allocation. |
| `acceldev.h` | Device interface header **provided by the course** — used unmodified, as required. |

Design notes:

- **Command feed.** Runs are submitted with `ACCELDEV_DEVICE_CMD_TYPE_RUN`. When there is no room
  in `CMD_MANUAL_FEED`, a dedicated executor kthread (`acceldev_executor.c`) holds the pending
  runs and feeds them as space frees up, so `ioctl(RUN)` does not block on a full feed.
- **Fences / waiting.** Waiting on a fence uses `ACCELDEV_DEVICE_CMD_TYPE_FENCE` and a device
  interrupt; the IRQ handler wakes the per-device `fence_queue`, and `ioctl(WAIT)` sleeps on it
  interruptibly.
- **mmap.** Buffer pages are DMA-allocated and exposed to user space lazily through a `fault`
  handler (`virt_to_page` + `get_page`), following the interface's page layout.
- **Error handling.** Any device error (command or memory) triggers an interrupt that restarts
  the device and marks all contexts as failed; subsequent `RUN`/`WAIT` on a failed context return
  `EIO`.

## Building

Standard out-of-tree kernel module build against the lab kernel:

```bash
make            # builds acceldev.ko
make install    # installs the module
```

## Tests

`tests/` contains the public tests shipped with the assignment. `tests/run.sh` runs the suite.

## Known limitations

This was submitted incomplete. Buffer release does not insert extra fence commands, so it does
not fully guarantee that all runs on the context up to that point have finished before the buffer
is freed. The locking is not fully hardened and some race conditions are likely. Because context
configs are effectively read-only, a single failed context cannot be reset independently, so the
driver restarts the whole device and fails every context on error. At submission time three of
the public tests failed (`tests/run.sh` excludes them); the rest passed.
